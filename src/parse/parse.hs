module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
    args <- getArgs
    line <- getLine
    putStrLn $ readExpr line
    main

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value!"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ ( parseEscapeQuote <|> (char 'd') <|> (char 'm'))
                 -- x <- many $ ( (string "div") <|> (noneOf "\""))
                 char '"'
                 return $ String x

parseEscapeQuote :: Parser Char
parseEscapeQuote = do x <- string "\\\""
                      return '\"'

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

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber







