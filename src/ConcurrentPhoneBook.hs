module ConcurrentPhoneBook
  ( initPhoneBook
  , addNumber
  , delPhoneBook
  , main
  )
  where

import Control.Concurrent
import Control.Exception (try)
import Control.Monad
import System.Directory (removeFile)
import System.FilePath (FilePath)
import Text.Printf (printf)

data PhoneBook = PhoneBook FilePath (MVar ())

initPhoneBook :: FilePath -> IO PhoneBook
initPhoneBook path = do
  m <- newEmptyMVar
  writeFile path ""
  putMVar m ()
  return $ PhoneBook path m

addNumber :: PhoneBook -> String -> String -> IO ()
addNumber (PhoneBook path m) name contact = do
  {-
    this is thread safe with the use of MVars, otherwise we will see:

    app: /tmp/phonebook: openFile: resource busy (file is locked)
    app: /tmp/phonebook: openFile: resource busy (file is locked)
    app: app: app: /tmp/phonebook: openFile: resource busy (file is locked)app: app: /tmp/phonebook: openFile: resource busy (file is locked)/tmp/phonebook: openFile: resource busy (file is locked)
    /tmp/phonebook: openFile: resource busy (file is locked)/tmp/phonebook: openFile: resource busy (file is locked)



    app: app: app: /tmp/phonebook: openFile: resource busy (file is locked)/tmp/phonebook: openFile: resource busy (file is locked)/tmp/phonebook: openFile: resource busy (file is locked)


    app: /tmp/phonebook: openFile: resource busy (file is locked)
    app: /tmp/phonebook: openFile: resource busy (file is locked)
    app: /tmp/phonebook: openFile: resource busy (file is locked)
  -}
  _ <- takeMVar m -- acquire lock
  -- we can also use appendFile here, but i just wanted to try out an explicit read + write operation
  contents <- readFile path
  putStrLn $ printf "length of file: % s" (show $ length contents)
  writeFile path $ printf "%s\n%s,%s" contents name contact
  putStrLn $ printf "added %s:%s" name contact
  putMVar m () -- unlock

delPhoneBook :: PhoneBook -> IO ()
delPhoneBook (PhoneBook path _) = do
  removeFile path

-- test the concurrency here by splitting out 1000 lightweight threads for writing
-- results are interesting... written not in order but all 1000 numbers are written
{-
shawn,1
shawn,2
shawn,3
shawn,4
shawn,5
shawn,7
shawn,9
shawn,6
shawn,8
shawn,93
shawn,94
shawn,95
shawn,96
shawn,97
shawn,98
shawn,99
shawn,100
shawn,101
shawn,102
shawn,103
shawn,104
shawn,105
shawn,106
shawn,13
shawn,19
shawn,25
shawn,31
shawn,37
shawn,43
-}
main :: IO ()
main = do
  pb <- initPhoneBook "/tmp/phonebook"
  forM_ [1..1000] $ \i -> forkIO $ addNumber pb "shawn" $ show i
  threadDelay 30000000
  delPhoneBook pb
