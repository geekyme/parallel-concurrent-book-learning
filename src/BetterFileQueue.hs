module BetterFileQueue (
  main
) where

import Data.Persistent.Collection (RefQueue, getQRef, pushSTM, popSTM)
import Control.Concurrent (forkIO, threadDelay)
import Data.TCache (atomicallySync)
import Control.Monad (forM_, forever)

-- NOTES: we can't have 2 different handlers of q like this and make q1 automatically pop what was pushed to q2.
--
-- let q1 = getQRef "parallelconc" :: RefQueue String
-- pop q >>= putStrLn
-- let q2 = getQRef "parallelconc" :: RefQueue String
-- push q2 $ show num

-- NOTES: we also need to call syncCache to save the in memory queue state to disk
main = do
  let q = getQRef "parallelconc" :: RefQueue String
  let items = [1..10000]
  forM_ items $ \num -> forkIO $ do
    atomicallySync $ pushSTM q $ show num
  forM_ items $ \_ -> forkIO $ do
    (atomicallySync $ popSTM q) >>= putStrLn
  threadDelay 3000000

