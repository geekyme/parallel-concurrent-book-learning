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
