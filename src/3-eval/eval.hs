module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = getLine >>= (putStrLn . show . eval . readExpr) >> main

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

-- PARSING

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ ((try parseEscape) <|> (noneOf "\""))
                 char '"'
                 return $ String x

parseEscape :: Parser Char
parseEscape = do char '\\'
                 x <- oneOf "nrt\\\""
                 return x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                             "#t" -> Bool True
                             "#f" -> Bool False
                             otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseNumberDo :: Parser LispVal
parseNumberDo = do x <- many1 digit
                   return $ Number $ read x

parseNumberBind :: Parser LispVal
parseNumberBind = (many1 digit) >>= (\ x -> return $ Number $ read x)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
                  head <- endBy parseExpr spaces
                  tail <- char '.' >> spaces >> parseExpr
                  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseString 
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList 
               char ')'
               return x

-- EVALUATION

showVal :: LispVal -> String
showVal (Atom s) = s
showVal (List l) = "(" ++ (unwordsList l) ++ ")"
showVal (DottedList l v) = "(" ++ (unwordsList l) ++ " . " ++ showVal v ++ ")"
showVal (Number n) = show n
showVal (String s) = "\"" ++ s ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", typeTest "symbol"),
              ("string?", typeTest "string"),
              ("number?", typeTest "number")
             ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

typeTest :: String -> [LispVal] -> LispVal
typeTest "symbol" [Atom _] = Bool True
typeTest "string" [String _] = Bool True
typeTest "number" [Number _] = Bool True
typeTest _ _ = Bool False























