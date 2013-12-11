
module ParseLisp where

import Control.Monad
import Text.ParserCombinators.Parsec
import System.Environment
import Numeric
import Control.Monad.Trans.State
import Control.Monad.Error
import Control.Monad.Trans.Class

data LispVal = Atom String
             | List [LispVal]
             | Numberi Integer
             | Numberf Float
             | String String
             | Bool Bool
             | Proc LispFn
             | Term String
             | Lambda [LispVal] [String]

type LispErr = ErrorT String IO
type Env = [(String,LispVal)]
type CalcRslt = StateT Env LispErr LispVal
type LispFn = [LispVal] -> CalcRslt

instance Show LispVal where
    show (Atom atom)   =  atom
    show (Numberi int) =  show int
    show (Numberf flt) =  show flt
    show (String str)  =  show str
    show (Bool bool)   =  show bool
    show (List lst)    =  case lst of
                                [] -> "()"
                                _  -> case head lst of
                                            Atom "quote" -> "'" ++ shLsp (tail lst)
                                            _            -> "(" ++ (shLsp lst) ++ ")"
                                      where shLsp lst = case lst of
                                                          []     -> ""
                                                          (y:[]) -> show y
                                                          (x:xs) -> show x ++ " " ++ (shLsp xs)
    show (Proc fn)     = "#Special form"
    show (Lambda _ _)  = "#Procedure"
    show (Term tm)     = tm

find :: Eq a => [a] -> a -> Bool             
find [] ch      = False
find (x:xs) ch  = if x==ch then True
                           else find xs ch

mReadFloat :: String -> Maybe Float
mReadFloat s = case readSigned readFloat s of
                    (v,r):[] -> if r == "" then Just v
                                           else Nothing
                    _        -> Nothing

termSym :: Parser ()
termSym = eof <|> do x <- oneOf " .()"
                     return ()

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseEscChar :: Parser Char
parseEscChar = do
    char '\\'
    t <- char '"'
    return t

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (try parseEscChar <|> noneOf "\"" )
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do first <- oneOf "+-." <|> digit
                 x <- if find "+-." first then many1 (digit <|> char '.')
                                          else many (digit <|> char '.')
                 let v = case first of
                            '+' -> '0':x
                            '-' -> '-':'0':x
                            '.' -> '0':'.':x
                            _   -> first:x
                 let v2 = if last v == '.' then v ++ "0"
                                           else v
                 if (find v2 '.') then case mReadFloat v2 of
                                            Just a -> return $ Numberf a
                                            Nothing -> fail "Read number error."
                                  else return $ (Numberi . read) v2
-- liftM (Number . read) $ many1 (digit <|> char '.')
                
parseList :: Parser LispVal
parseList = do char '('
               spaces
               x <- endBy parseExpr1 spaces
               char ')'
               return $ List x

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr1
    return $ List [Atom "quote", x]

parseExpr1 :: Parser LispVal
parseExpr1 = (try parseNumber)
             <|> parseString
             <|> parseList
             <|> parseQuoted
             <|> parseAtom

parseExpr :: Parser (LispVal,String)
parseExpr = do  (_,_) <- do spaces
                            return (String "","")
                (lv,_) <- do x <- (try parseNumber)
                                  <|> parseString
                                  <|> parseList
                                  <|> parseQuoted
                                  <|> parseAtom
                             return (x,"")
                (_,s)  <- do r <- manyTill anyChar (try termSym) --remaining input
                             return (String r,r)
                return (lv,s)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right (v,r) -> "left: " ++ r ++ ", " ++
        case v of
            Bool bool -> "Bool Found value"
            Atom atom -> "Atom found"
            String str -> "String found"
            Numberi num -> "Int found" ++ show num
            Numberf flt -> "Float found" ++ show flt
            List lst -> "List found"

readExpr1 :: String -> Either String LispVal
readExpr1 input = case parse parseExpr "lisp" input of
    Left err -> Left $ show err
    Right (v,r) -> Right v

printLisp :: LispVal -> IO ()
printLisp lsp = do case lsp of
                    Atom atom   ->  putStr(atom)
                    Numberi int ->  putStr(show int)
                    Numberf flt ->  putStr(show flt)
                    String str  ->  putStr(show str)
                    Bool bool   ->  putStr(show bool)
                    List lst    ->  do  putStr "("
                                        printLisp' lst
                                        putStr ")"
                                        where printLisp' lst = case lst of
                                                                (y:[]) -> printLisp y
                                                                (x:xs) -> do printLisp x
                                                                             putStr " "
                                                                             printLisp' xs
                    Proc fn     -> putStr("#Procedure!")
                    Term tm     -> putStr ""