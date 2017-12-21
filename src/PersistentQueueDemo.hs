module PersistentQueueDemo (
  main
) where

import Data.Persistent.Collection
import Control.Concurrent
import Data.TCache
import Control.Monad

-- TODO: the atomic stuff also doesn't seem to be working as expected. output:
{-

1
23456789111111111122222222333334444555556666677777888
2233333444488889999999999144555556666677777888






0123456789012345791357913580246802468024680246802468024667890123456789079135791357913579135


-}
-- NOTES: we can't have 2 different handlers of q like this and make q1 automatically pop what was pushed to q2.
--
-- let q1 = getQRef "parallelconc" :: RefQueue String
-- pop q >>= putStrLn
-- let q2 = getQRef "parallelconc" :: RefQueue String
-- push q2 $ show num

-- NOTES: we also need to call syncCache to save the in memory queue state to disk
main = do
  let q = getQRef "parallelconc" :: RefQueue String
  forM_ [1..100] $ \_ -> forkIO $ do
    threadDelay 1000000
    (atomically $ popSTM q) >>= putStrLn
  forM_ [1..100] $ \num -> forkIO $ do
    threadDelay 1000000
    atomically $ pushSTM q $ show num
  threadDelay 10000000
  syncCache
