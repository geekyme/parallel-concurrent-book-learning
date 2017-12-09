module C7 where

import Control.Concurrent
import Control.Monad
import System.IO

basic :: IO ()
basic = do
  hSetBuffering stdout NoBuffering
  forkIO $ replicateM_ 100000 (putChar 'A')
  replicateM_ 100000 (putChar 'B')

reminders :: IO ()
reminders = forever $ do
  t <- read <$> getLine :: IO Int
  forkIO $ do
    threadDelay (t * 1000000)
    putStrLn "reminder elapsed."

mvarbasic :: IO ()
mvarbasic = do
  m <- newEmptyMVar
  forkIO $ putMVar m 'x'
  r <- takeMVar m
  print r

mvar2 :: IO ()
mvar2 = do
  m <- newEmptyMVar
  forkIO $ do
    putMVar m 'x'
    putMVar m 'y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r

-- Logging service using mvar

data Logger = Logger (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          putStrLn msg
          loop
        Stop s -> do
          putStrLn "logger: stop"
          putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) msg = do
  putMVar m (Message msg)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  stopper <- newEmptyMVar
  putMVar m (Stop stopper)
  takeMVar stopper
