module Main where

main :: IO ()
main = do
  putStrLn "Haskell Features Example"
  putStrLn "--------------------------"
  putStrLn "This demonstrates basic Haskell features:"
  putStrLn "- Pure functions"
  putStrLn "- Pattern matching"
  putStrLn "- Type system"
  putStrLn "- Recursion"
  putStrLn "- Lazy evaluation"
  putStrLn ""
  print (factorial 5)  -- Example of recursion and pattern matching
  print (add 2 3)      -- Example of pure function
  print (safeHead [1,2,3])  -- Example of pattern matching

-- Pure function example
add :: Int -> Int -> Int
add x y = x + y

-- Factorial with pattern matching
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Safe head with pattern matching
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- monad example
import Control.Monad (when)
import Data.Maybe (fromMaybe)

-- Example of using Maybe monad
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a `div` b)
-- Example of using the Maybe monad
mainWithMaybe :: IO ()
mainWithMaybe = do
  let result = safeDivide 10 2
  case result of
    Just value -> putStrLn $ "Result: " ++ show value
    Nothing -> putStrLn "Division by zero"
  putStrLn "Using Maybe monad for safe division"

