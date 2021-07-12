{-# LANGUAGE ExistentialQuantification #-}
module Main where
import System.IO hiding (try)
import Control.Monad
import Control.Monad.Error
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

----------
-- REPL --
----------

main :: IO ()
main = do
    args <- getArgs
    case length args of
      0 -> runRepl
      1 -> evalAndPrint $ args !! 0
      otherwise -> putStrLn "too many arguments!"
-- main = do
--     line <- getLine
--     evaled <- return $ liftM show $ readExpr line >>= eval
--     putStrLn $ extractValue $ trapError evaled
--     main

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
       then return ()
       else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "$> ") evalAndPrint


-------------
-- PARSING --
-------------

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

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

----------------
-- EVALUATION --
----------------

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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = ifEval pred conseq alt
eval (List (Atom "cond" : xs)) = cond xs
eval (List (Atom "case" : keyform : keys)) = caseEval (eval keyform) keys
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form: " badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "unrecognized primitive function args" func) 
                        ($ args) $ lookup func primitives

ifEval :: LispVal -> LispVal -> LispVal -> ThrowsError LispVal
ifEval pred conseq alt = 
                     do result <- eval pred
                        case result of
                          Bool False -> eval alt
                          Bool True -> eval conseq
                          notBool -> throwError $ TypeMismatch "bool" notBool

cond :: [LispVal] -> ThrowsError LispVal
cond [] = return $ Bool False
cond (x:xs) =
    case x of
      (List [pred, conseq]) ->
        do result <- eval pred
           case result of
             Bool False -> cond xs
             Bool True -> eval conseq
             notBool -> throwError $ TypeMismatch "bool" notBool
      badArg -> throwError $ TypeMismatch "(predicate consequence)" badArg

caseEval :: (ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
caseEval keyform [] = return $ Bool False
caseEval keyform (x:xs) =
    case x of
      (List [key, conseq]) ->
        do  kf <- keyform
            result <- eqv [kf, key]
            case result of
              Bool False -> caseEval keyform xs
              Bool True -> eval conseq
              notBool -> throwError $ TypeMismatch "bool" notBool
      badArg -> throwError $ TypeMismatch "(predicate consequence)" badArg



-- Primitives
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+",     numericBinop (+)),
              ("-",     numericBinop (-)),
              ("*",     numericBinop (*)),
              ("/",     numericBinop div),
              ("mod",   numericBinop mod),
              ("quotient",  numericBinop quot),
              ("remainder", numericBinop rem),
              ("=",     numBoolBinop (==)),
              ("<",     numBoolBinop (<)),
              (">",     numBoolBinop (>)),
              ("/=",    numBoolBinop (/=)),
              ("<=",    numBoolBinop (<=)),
              (">=",    numBoolBinop (>=)),
              ("&&",    boolBoolBinop (&&)),
              ("||",    boolBoolBinop (||)),
              ("string=?",  strBoolBinop (==)),
              ("string?",   strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eqv?", eqv),
              ("equal?", equal),
              ("symbol?",   typeTest "symbol"),
              -- ("string?", typeTest "string"),
              ("number?",   typeTest "number")
             ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

-- Unpackers
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n])    = unpackNum n
unpackNum notNum        = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String str)  = return str
unpackStr (Number n)    = return $ show n
unpackStr (Bool b)      = return $ show b
unpackStr notStr        = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do unpacked1 <- unpacker arg1
       unpacked2 <- unpacker arg2
       return $ unpacked1 == unpacked2 
       `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x) 
equal badArgList = throwError $ NumArgs 2 badArgList

-- List Funcs
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg]    = throwError $ TypeMismatch "pair" badArg
car badArgList  = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [DottedList [xs] x] = return x
cdr [badArg]    = throwError $ TypeMismatch "pair" badArg
cdr badArgList  = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]   = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- Equalities
eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool   x, Bool   y] = return $ Bool $ x == y
eqv [Number x, Number y] = return $ Bool $ x == y
eqv [String x, String y] = return $ Bool $ x == y
eqv [Atom   x, Atom   y] = return $ Bool $ x == y
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return $ Bool $ 
    (length arg1 == length arg2) &&
        (and $ map eqvPair $ zip arg1 arg2)
            where eqvPair (x1, x2) = case eqv [x1, x2] of
                                       Left err -> False
                                       Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


typeTest :: String -> [LispVal] -> ThrowsError LispVal
typeTest "symbol" [Atom _]   = return $ Bool True
typeTest "string" [String _] = return $ Bool True
typeTest "number" [Number _] = return $ Bool True
typeTest _ _                 = return $ Bool False

--------------------
-- ERROR HANDLING --
--------------------

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname 
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func 
showError (NumArgs expected found) = "Expected: " ++ show expected 
    ++ " args: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ show expected
    ++ ", found " ++ show found
showError (Parser parseErr) = "Parse Error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val















