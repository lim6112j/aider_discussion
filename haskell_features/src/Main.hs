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
contTExample = ContT $ \k -> do
  putStrLn "Performing some action"
  k 10

-- Combining transformers
type CombinedTransformer = StateT Int (ReaderT String (WriterT String IO))

combinedExample :: Int -> String -> CombinedTransformer Int
combinedExample _initialState = do -- Renamed initialState as it's not used directly after the liftIO line
  lift $ lift $ tell "Starting combined example. " -- Added space for clarity
  inputString <- lift ask -- Get the string from ReaderT
  liftIO $ putStrLn $ "Input String from ReaderT: " ++ inputString
  put 5
  currentState <- get
  liftIO $ putStrLn $ "Set State to: " ++ show currentState
  modify (+ 10) -- Modify state
  newState <- get
  lift $ tell $ "Modified state to: " ++ show newState ++ ". " -- Log modification
  return newState -- Return the final state

-- Runner for the combined example
combinedRunner :: String -> IO ((Int, Int), String)
combinedRunner str = do
  ((resultCombined, finalState), logs) <- runWriterT (runReaderT (runStateT (combinedExample 0 str) 0) str)
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
