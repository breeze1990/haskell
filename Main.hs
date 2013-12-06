
module Main where

import EvalLisp
import ParseLisp
import System.IO
import Control.Monad.State
import Control.Monad.Error

cmd :: Env -> IO()
cmd cEnv = do --putStrLn $ show cEnv
              out <- runErrorT (runStateT repl cEnv)
              case out of
                 Left errMsg -> do putStrLn $ "ERR: " ++ errMsg
                                   cmd cEnv
                 Right (val,newEnv) ->    case val of
                                               Term msg -> if msg == "Bye!" then putStrLn msg
                                                                            else cmd newEnv
                                               _        -> do putStrLn $ show val
                                                              cmd newEnv

main :: IO ()
main = cmd initEnv
                                               
repl = do liftIO $ putStr "> "
          liftIO $ hFlush stdout
          x <- liftIO $ getLine
          case readExpr1 x of
              Left err -> do liftIO $ putStrLn ("No match: " ++ err)
                             repl
              Right v  -> eval v
                                        
                                   