module Conc where

import Control.Concurrent
import Control.Monad
import Control.Exception
import qualified Data.Map as Map

main :: IO ()
main = do
  m <- newEmptyMVar
  forkIO $ takeMVar m >>= putStrLn
  putMVar m "hi"
  return ()
  
data ThreadStatus = Running | Finished | Threw IOException
                    deriving (Show)

newtype ThreadManager = ThreadManager (MVar (Map.Map ThreadId (MVar ThreadStatus)))

newManager :: IO ThreadManager
newManager = liftM ThreadManager $ newMVar Map.empty

forkManagedThread :: ThreadManager -> IO () -> IO ThreadId
forkManagedThread (ThreadManager mapMVar) x = do
        state <- newEmptyMVar
        tid <- forkIO $ do -- forkIO :: IO () -> IO ThreadId
            result <- try x -- try :: Exception e => IO a -> IO (Either e a)
            putMVar state $ either Threw (const Finished) result
        m <- takeMVar mapMVar
        putMVar mapMVar $ Map.insert tid state m
        return tid
