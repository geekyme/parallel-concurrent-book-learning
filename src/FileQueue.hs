{-
  POC a persistent queue
   - persist state to disk
   - allow multiple readers and writers
   - in order processing
  TODO:
   - this needs to handle restarts, read the stuff in todo/ doing/ and add them to the channels
   - this also needs to handle a more reliable test
-}

module FileQueue
  ( initQueue
  , add
  , start
  , main
  )
  where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Chan as Chan
import Control.Monad (forM_)
import System.Directory as Dir
import System.Environment (getArgs)
import System.FilePath (FilePath, (</>))

type Lock = MVar.MVar ()
data Queue a = Queue
  { todo :: Chan.Chan a -- ^ all items added
  , doing :: Chan.Chan a -- ^ all items started, should be removed from here once completed
  , path :: FilePath -- ^ file path to persist the queue
  , lock :: Lock -- ^ Using this to coordinate all operations on the queue
  }

initQueue :: FilePath -> IO (Queue a)
initQueue p = do
  Dir.createDirectoryIfMissing True $ todoPath p
  Dir.createDirectoryIfMissing True $ doingPath p
  t <- Chan.newChan
  d <- Chan.newChan
  l <- MVar.newMVar ()
  return $ Queue t d p l

add :: Show a => Queue a -> a -> IO ()
add (Queue t _ p l) i =
  atomic l $ do
    writeFile (todoPath p </> show i) ""
    Chan.writeChan t i

{-
  1. grab the job from todo
  2. add to doing
  3. persist queue state to file
  4. do the job
  5. remove from doing
  4. persist the queue state to file
-}
start :: Show a => Queue a -> (a -> IO ()) -> IO ()
start (Queue t d p l) action =
  atomic l $ do
    i <- Chan.readChan t
    Chan.writeChan d i
    Dir.renamePath (todoPath p </> show i) (doingPath p </> show i)
    action i
    i <- Chan.readChan d
    removeFile (doingPath p </> show i)

endQueue :: Queue a -> IO ()
endQueue (Queue _ _ p _) = do
  Dir.removeDirectoryRecursive p

{-
  this is interesting, it ensures that the queue is enqueuing and dequeueing in order.

  Without atomic:
    added 3
    added 4
    added 1
    added 2
    added 5
    added 6
    started 1
    added 7
    added 8
    started 3
    started 4
    added 10
    started 2
    started 5
    added 9
    started 10
    started 6
    started 8
    started 7
    started 9

  With atomic:
    added 3
    added 1
    added 2
    added 4
    added 6
    added 5
    added 7
    started 3
    started 1
    started 2
    started 4
    added 10
    started 6
    started 5
    started 7
    added 8
    added 9
    started 10
    started 8
    started 9
-}
atomic :: Lock -> IO a -> IO a
atomic l action = do
  baton <- MVar.takeMVar l
  res <- action
  MVar.putMVar l baton
  return res

todoPath :: FilePath -> FilePath
todoPath path = path </> "todo"

doingPath :: FilePath -> FilePath
doingPath path = path </> "doing"

main :: IO ()
main = do
  max <- (read . head <$> getArgs) :: IO Int
  q <- initQueue "/tmp/q"
  let items = [1..max]
  forM_ items $ \i -> forkIO $ do
    add q i
    putStrLn $ "added " ++ show i
  forM_ items $ \i -> forkIO $ do
    start q (\i -> putStrLn $ "started " ++ show i)
  threadDelay 5000000
  endQueue q

