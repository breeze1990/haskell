
module Environ where

import ParseLisp

updEnv :: [String] -> [LispVal] -> Env -> Env
updEnv [] [] e = e
updEnv (fs:rs) (fl:rl) e = (fs,fl):(updEnv rs rl e)

extEnv :: (String,LispVal) -> Env -> Env
extEnv p e = p:e

appEnv :: String -> Env -> Maybe LispVal
appEnv s [] = Nothing
appEnv s (x:xs) = let (s1,v1) = x in
                      if s1 == s then Just v1
                                 else appEnv s xs

xstEnv :: String -> Env -> Bool
xstEnv s [] = False
xstEnv s ((sym,v):xs) = if sym == s then True
                                    else xstEnv s xs
                                    
mdfEnv :: (String,LispVal) -> Env -> Env
mdfEnv _ [] = []
mdfEnv (s,lv) ((sym,lvt):xs) = if s == sym then ((s,lv):xs)
                                           else ((sym,lvt):(mdfEnv (s,lv) xs))
