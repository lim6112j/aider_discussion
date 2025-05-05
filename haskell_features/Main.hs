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
