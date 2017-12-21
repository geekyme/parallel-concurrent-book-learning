module PersistentQueueDemo (
  main
) where

import Data.Persistent.Collection
import Control.Concurrent
import Data.TCache
import Control.Monad

-- TODO: this currently doesn't work well because the push / pops are non-atomic i suspect
{-
1
2
3
4
5
6
7
8
9111911
99111111222222222233333333334444444444555555555566666666667777777777888888888999999980128037945678901234567890123456789012345678901234567890123456789012345678901235678901234564



0
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
    pop q >>= putStrLn
  forM_ [1..100] $ \num -> forkIO $ do
    threadDelay 1000000
    push q $ show num
  threadDelay 10000000
  syncCache
