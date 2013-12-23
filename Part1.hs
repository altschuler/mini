module Main where
import Data.List
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Hello, " ++ (intercalate " and " args) ++ ", give me a number"
  num1 <- getLine
  putStrLn "and one more"
  num2 <- getLine
  putStrLn $ "their product is " ++ show ((read num1 :: Int) * read num2)
  
  
