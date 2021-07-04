module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
    args <- getArgs
    let arg0 = readIntArg args 0
    let arg1 = readIntArg args 1
    putStrLn ((++) "Hello, " $ show $ (+) arg0 arg1)
    line <- getLine
    putStrLn ("Henlo: " ++ line)

readIntArg :: [String] -> Int -> Int
readIntArg args i = read $ args !! i
