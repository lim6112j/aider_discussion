module Main where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Cont
import Control.Monad.Trans

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
contTExample = \k -> do
  putStrLn "Performing some action"
  k 10

-- Combining transformers
type CombinedTransformer = StateT Int (ReaderT String (WriterT String IO))

combinedExample :: String -> CombinedTransformer Int
combinedExample initialString = do
  tell "Starting combined example"
  put 5
  string <- liftIO $ return initialString
  liftIO $ putStrLn $ "String from ReaderT: " ++ string
  get >>= \state -> liftIO $ putStrLn $ "State from StateT: " ++ show state
  return 10

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
  resultCombined <- runStateT
    (runReaderT
      (runWriterT (combinedExample "Initial String"))
      "Reader String")
    0
  print resultCombined
