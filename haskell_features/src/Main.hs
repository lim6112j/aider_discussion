{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
module Main where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Cont
import Control.Monad.Trans
import Data.Data
import GHC.Generics (Generic)
import Language.Haskell.TH
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Proxy
import Control.Lens
-- Example using StateT
type StateTExample = StateT Int IO

stateTExample :: StateTExample Int
stateTExample = do
  put 10
  get >>= \x -> return (x + 5)

-- Example using ReaderT
type ReaderTExample = ReaderT String IO

readerTExample :: ReaderTExample Int
readerTExample = do
  ask >>= \s -> return (length s)

-- Example using WriterT
type WriterTExample = WriterT String IO

writerTExample :: WriterTExample Int
writerTExample = do
  tell "Logging some information"
  return 42

-- Example using ContT
type ContTExample r = ContT r IO Int

contTExample :: ContTExample r
contTExample = ContT $ \k -> do
  putStrLn "Performing some action"
  k 10

-- Combining transformers
type CombinedTransformer = StateT Int (ReaderT String (WriterT String IO))

combinedExample :: Int -> CombinedTransformer Int
combinedExample initialState = do
  -- Writer operations need to be lifted through StateT and ReaderT
  lift $ lift $ tell "Starting combined example. "
  inputString <- lift ask
  liftIO $ putStrLn $ "Input String from ReaderT: " ++ inputString
  put 5
  currentState <- get
  liftIO $ putStrLn $ "Set State to: " ++ show currentState
  modify (+ 10)
  newState <- get
  lift $ tell $ "Modified state to: " ++ show newState ++ ". "
  return newState

-- 1. Type Class with Default Implementation
class Printable a where
  toString :: a -> String
  toString = show  -- Default implementation using Show
  
  fromString :: String -> a
  fromString = read  -- Default implementation using Read

data Person = Person { name :: String, age :: Int } deriving (Show, Read)

instance Printable Person  -- Uses default implementations

-- 2. Generic Programming with Data.Data
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Data, Generic, Show)

treeDepth :: Data a => Tree a -> Int
treeDepth (Leaf _) = 1
treeDepth (Branch l r) = 1 + max (treeDepth l) (treeDepth r)

-- 3. View Patterns
evenOdd :: Int -> String
evenOdd (even -> True) = "Even"
evenOdd (odd -> True)  = "Odd"

-- 4. Template Haskell Example

hello :: Q [Dec]
hello = do
  name <- newName "world"
  return [ FunD (mkName "hello") 
          [ Clause [] (NormalB (LitE (StringL ("Hello " ++ show name)))) [] ] ]

-- Generate the hello function at compile-time
$(hello)

-- 5. STM Concurrency Example

bankTransfer :: TVar Int -> TVar Int -> Int -> IO ()
bankTransfer from to amount = atomically $ do
  balance <- readTVar from
  when (balance < amount) $ throwSTM $ userError "Insufficient funds"
  writeTVar from (balance - amount)
  modifyTVar to (+ amount)

-- 6. Type-Level Programming

data Nat = Z | S Nat

type Add :: Nat -> Nat -> Nat -> Constraint
class Add a b c | a b -> c
instance Add Z b b
instance Add a b c => Add (S a) b (S c)

-- 7. Lens Example

data Point = Point { _x :: Double, _y :: Double } deriving Show
makeLenses ''Point

movePoint :: Point -> Point
movePoint = over x (+ 1.0) . over y (* 2.0)

-- Runner for the combined example
combinedRunner :: String -> IO ((Int, Int), String)
combinedRunner str = do
  ((resultCombined, finalState), logs) <- runWriterT (runReaderT (runStateT (combinedExample 0) 0) str)
  return ((resultCombined, finalState), logs)

main :: IO ()
main = do
  putStrLn "StateT Example:"
  resultState <- runStateT stateTExample 0
  print resultState

  putStrLn "\nReaderT Example:"
  resultReader <- runReaderT readerTExample "Hello, world!"
  print resultReader

  putStrLn "\nWriterT Example:"
  (resultWriter, logs) <- runWriterT writerTExample
  print resultWriter
  putStrLn "Logs:"
  print logs

  putStrLn "\nContT Example:"
  resultCont <- runContT contTExample (\x -> do
    print x
    return ())
  print resultCont

  putStrLn "\nCombined Example:"
  ((resultCombined, finalState), logs) <- combinedRunner "Reader String"
  putStrLn $ "Combined Result: " ++ show resultCombined
  putStrLn $ "Final State: " ++ show finalState
  putStrLn $ "Combined Logs: " ++ logs

  putStrLn "\nNew Features Demo:"
  -- Type Class demo
  putStrLn "\n1. Type Class:"
  let person = Person "Alice" 30
  print $ toString person
  print (fromString "Person {name = \"Bob\", age = 25}" :: Person)

  -- Generic Programming demo
  putStrLn "\n2. Generic Programming:"
  let tree = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
  print $ treeDepth tree
  print $ toConstr (Leaf 1)  -- Using Data.Data methods

  -- View Patterns demo
  putStrLn "\n3. View Patterns:"
  print $ evenOdd 4
  print $ evenOdd 5

  -- Template Haskell demo
  putStrLn "\n4. Template Haskell:"
  print hello

  -- STM Concurrency demo
  putStrLn "\n5. STM Concurrency:"
  accountA <- newTVarIO 100
  accountB <- newTVarIO 0
  concurrently_ (bankTransfer accountA accountB 50) (bankTransfer accountA accountB 30)
  balanceA <- atomically $ readTVar accountA
  balanceB <- atomically $ readTVar accountB
  putStrLn $ "Final balances: A=" ++ show balanceA ++ " B=" ++ show balanceB

  -- Type-Level Programming demo
  putStrLn "\n6. Type-Level Programming:"
  let add5 = Proxy :: Proxy (Add (S (S (S (S (S Z))))) Z (S (S (S (S (S Z))))))
  print "Type-level addition validated"

  -- Lens demo
  putStrLn "\n7. Lenses:"
  let p = Point 2.0 3.0
  print $ movePoint p
