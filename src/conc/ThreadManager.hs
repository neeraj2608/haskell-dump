module ThreadManager (
  newManager,
  getStatus,
  waitFor,
  waitAll,
  forkManagedThread) where

import Control.Monad
import Control.Exception
import Control.Concurrent
import qualified Data.Map as Map
  
data ThreadStatus = Running | Finished | Threw IOException
                    deriving (Show)

newtype ThreadManager = ThreadManager (MVar (Map.Map ThreadId (MVar ThreadStatus)))

newManager :: IO ThreadManager
newManager = liftM ThreadManager $ newMVar Map.empty

forkManagedThread :: ThreadManager -> IO () -> IO ThreadId
forkManagedThread (ThreadManager mapMVar) x =
        -- modifyMVar will put the original value back in the MVar
        -- should an exception occur
        modifyMVar mapMVar (\oldMap -> do -- MVar a -> (a -> IO (a, b)) -> IO b
          state <- newEmptyMVar
          tid <- forkIO $ do -- forkIO :: IO () -> IO ThreadId
              result <- try x -- try :: Exception e => IO a -> IO (Either e a)
              putMVar state $ either Threw (const Finished) result -- the end result of this is that as long the thread is running,
                                                                   -- the state MVar will not have anything in it. When the
                                                                   -- thread has finished, the state MVar will have either
                                                                   -- a Threw Exception, or a Just Finished
          return (Map.insert tid state oldMap, tid))

getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (ThreadManager mapMVar) tid =
        modifyMVar mapMVar $ \oldMap -> do -- MVar a -> (a -> IO (a, b)) -> IO b
        let x = Map.lookup tid oldMap
        case x of
            Just tstatusMVar -> do
                ioMaybeStatus <- tryTakeMVar tstatusMVar -- tryTakeMVar :: MVar a -> IO (Maybe a) returns Nothing
                                                         -- if tstatusMVar is empty. as explained above,
                                                         -- tstatusMVar will be empty as long as the thread is
                                                         -- running
                case ioMaybeStatus of
                  Nothing -> return (oldMap, Just Running)
                  Just Running -> return (oldMap, ioMaybeStatus)
                  Just Finished -> do
                    let newMap = Map.delete tid oldMap
                    return (newMap, Nothing)
            Nothing -> return (oldMap, Nothing) -- the tid passed in isn't being managed at all

waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (ThreadManager mapMVar) tid =
        modifyMVar mapMVar $ \oldMap -> do -- MVar a -> (a -> IO (a, b)) -> IO b
        let (maybetstatus, newMap) = Map.updateLookupWithKey (\_ _ -> Nothing) tid oldMap -- Map.updateLookupWithKey :: (k -> a -> Maybe a) -> k -> Map.Map k a -> (Maybe a, Map.Map k a)
        case maybetstatus of
          Just s -> do
              status <- takeMVar s -- we will block here until the thread is
                                   -- done (successfully or unsuccessfully)
                                   -- And once the thread is done, we don't
                                   -- want the tid in the map, so we remove
                                   -- it (this "removal" is done by
                                   -- returning newMap, which had the
                                   -- corresponding entry set to Nothing)
              case status of
                Finished -> return (newMap, Just Finished)
                (Threw _) -> return (newMap, Just Finished)
          Nothing -> return (oldMap, Nothing) -- the tid passed in isn't being managed at all

waitAll :: ThreadManager -> IO ()
waitAll (ThreadManager mapMVar) =
        modifyMVar_ mapMVar $ \oldMap -> do -- modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
            mapM_ (waitFor (ThreadManager mapMVar)) (Map.keys oldMap) -- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
            return oldMap
