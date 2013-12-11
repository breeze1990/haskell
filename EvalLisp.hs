
module EvalLisp where

import StdFunctions
import Environ
import ParseLisp
import Control.Monad.State
import Control.Monad.Error

initEnv :: Env
initEnv = [("quit",Proc quit),("+",Proc add_a),("-",Proc sub_a),("define",Proc def_a),("=",Proc equ_a),("set!",Proc set),
            ("if",Proc iff_a),("quote",Proc quo_a),("lambda",Proc lbd_a),("cons",Proc cons_a),("car",Proc car_a),("cdr",Proc cdr_a)]

--(lambda (n) (proc))
app_proc :: LispVal -> [LispVal] -> CalcRslt
app_proc (Lambda fn var) args = do  oldEnv <- get
                                    vals <- mapM eval args
                                    if length var /= length vals then lift $ throwError " arguments number error. "
                                                                 else put $ updEnv var vals oldEnv
                                    nEnv <- get
                                    --liftIO $ putStrLn $ show nEnv
                                    ret <- exec fn
                                    put oldEnv
                                    return ret
                                    where exec (lst:[]) = eval lst
                                          exec (fst:rst)= do eval fst
                                                             exec rst
app_proc _ _ = lift $ throwError "Not a procedure"

createVarList :: [LispVal] -> Maybe [String]
createVarList [] = Just []
createVarList (Atom var:rst)= do let r = createVarList rst
                                 case r of
                                    Just rl -> Just (var:rl)
                                    Nothing -> Nothing
createVarList _ = Nothing

lbd_a :: LispFn
lbd_a (t:[]) = lift $ throwError "bad syntax: lacking body"
lbd_a (List var:body) = do let vlst = createVarList var
                           case vlst of
                                Nothing -> lift $ throwError "bad syntax: arguments error"
                                Just v  -> return $ Lambda body v
lbd_a _ = lift $ throwError " bad syntax for lambda "

car_a :: LispFn
car_a (t:[]) = do arg <- eval t
                  case arg of
                    List (head:rst) -> return head
                    _               -> lift $ throwError "Proc: car: error"
car_a _      = lift $ throwError "Proc: car: error"

cdr_a :: LispFn
cdr_a (t:[]) = do arg <- eval t
                  case arg of
                    List (head:rst) -> return $ List rst
                    _               -> lift $ throwError "Proc: cdr: error"
cdr_a _      = lift $ throwError "Proc: cdr: error"

cons_a  :: LispFn
cons_a (fst:snd:[]) = do head <- eval fst
                         tail <- eval snd
                         cons [head,tail]
cons_a t = cons t

quo_a :: LispFn
quo_a (t:[]) = return $ t
quo_a _ = lift $ throwError "Proc: Quote: Arguments error"

equ_a :: LispFn
equ_a (a1:a2:[])    = do v1 <- eval a1
                         v2 <- eval a2
                         equ [v1,v2]
equ_a t             = equ t

iff_a :: LispFn
iff_a (expr:stm1:stm2:[]) = do  ex <- eval expr
                                case ex of
                                    Bool b -> if b then eval stm1
                                                   else eval stm2
                                    _ -> lift $ throwError "Proc: if: 2nd arg should be Bool "
iff_a _                   = lift $ throwError "Proc: if: arguments error"


add_a :: LispFn
add_a (a1:a2:[])    = do v1 <- eval a1
                         v2 <- eval a2
                         add [v1,v2]
add_a t             = add t

sub_a :: LispFn
sub_a (a1:a2:[])    = do v1 <- eval a1
                         v2 <- eval a2
                         sub [v1,v2]
sub_a t             = sub t
                         
def_a :: LispFn
def_a (var:val:[]) = do out <- eval val
                        def [var,out]
def_a t            = def t

set :: LispFn
set (Atom var:val:[]) = do oldEnv <- get
                           if xstEnv var oldEnv then put $ mdfEnv (var,val) oldEnv
                                                else lift $ throwError "Proc:set!: symbol not found"
                           return $ Term "NULL"
                           
set _ = lift $ throwError "Proc:set!: argumen number error"

--eval :: LispVal -> LispVal
eval :: LispVal -> CalcRslt
eval (Numberi int) = return $ Numberi int
eval (Numberf flt) = return $ Numberf flt
eval (String str)  = return $ String str
eval (Bool bool)   = return $ Bool bool
eval (Atom atom) = do curE <- get
                      --liftIO $ putStrLn $ "DEBUG: " ++ atom ++" "++ show curE
                      case appEnv atom curE of
                          Just val -> return val
                          Nothing -> lift $ throwError ("symbol " ++ atom ++ " not found.")
eval (List lst) = if length lst == 0 then lift $ throwError ("error empty list")
                                     else do let (fst:arg) = lst
                                             fv <- eval fst
                                             case fv of
                                                Proc fn         -> fn arg
                                                Lambda body var -> app_proc fv arg
                                                _               -> lift $ throwError "Invalid list, not applicable."
