
module StdFunctions where

import ParseLisp
import Environ
import System.IO
import Control.Monad.State
import Control.Monad.Error

equ :: LispFn
equ (String s1:String s2:[])   = return $ Bool (s1 == s2)
equ (Bool b1:Bool b2:[])       = return $ Bool (b1 == b2)
equ (Numberi i1:Numberi i2:[]) = return $ Bool (i1 == i2)
equ (Numberf f1:Numberf f2:[]) = return $ Bool (f1 == f2)
equ (Numberi i1:Numberf f2:[]) = return $ Bool ((read $ show i1) == f2)
equ (Numberf f1:Numberi i2:[]) = return $ Bool (f1 == (read $ show i2))
equ _                          = return $ Bool False


sub :: LispFn
sub (n1:n2:[]) = do case n1 of
                        Numberi ni1 -> case n2 of
                                            Numberi ni2 -> subi ni1 ni2
                                            Numberf nf2 -> subf ni1 nf2
                                            _ -> lift thrErr
                        Numberf nf1 -> case n2 of
                                            Numberi ni2 -> subf nf1 ni2
                                            Numberf nf2 -> subf nf1 nf2
                                            _ -> lift thrErr
                        _ -> lift thrErr
                        where thrErr = throwError "Proc:-: argument type error"
                              subi a b = return $ Numberi (a - b)
                              subf a b = return $ Numberf ((read (show a)) - (read (show b)))
sub _ = lift $ throwError "Proc:-: argument number error"


add :: LispFn
add (n1:n2:[]) = do case n1 of
                        Numberi ni1 -> case n2 of
                                            Numberi ni2 -> addi ni1 ni2
                                            Numberf nf2 -> addf ni1 nf2
                                            _ -> lift thrErr
                        Numberf nf1 -> case n2 of
                                            Numberi ni2 -> addf nf1 ni2
                                            Numberf nf2 -> addf nf1 nf2
                                            _ -> lift thrErr
                        _ -> lift thrErr
                        where thrErr = throwError "Proc:+: argument type error"
                              addi a b = return $ Numberi (a + b)
                              addf a b = return $ Numberf ((read (show a)) + (read (show b)))
add _ = lift $ throwError "Proc:+: argument number error"

quit :: LispFn
quit _ = do liftIO $ putStrLn "Bye!"
            return $ Term "Bye!"

def :: LispFn
def (Atom var:val:[]) = do oldEnv <- get
                           if xstEnv var oldEnv then put $ mdfEnv (var,val) oldEnv
                                                else put $ extEnv (var,val) oldEnv
                           return $ Term "NULL"
                           
def _ = lift $ throwError "Proc:define: arguments error"

cons :: LispFn
cons (head:List ls:[]) = case head of
                            Term tm -> lift $ throwError "Cons: first argument error"
                            _ -> return $ List (head:ls)
cons _ = lift $ throwError "Cons: error"