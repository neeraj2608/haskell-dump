module Transform where
{-
- Using monad transformers
-}

import System.FilePath
import System.Directory
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

data AppConfig = AppConfig {maxDepthToGo :: Int}
                 deriving (Show)
data AppState = AppState {getMaxDepthReached :: Int}
                 deriving (Show)

type App = ReaderT AppConfig (StateT AppState IO)

run :: App a -> Int -> IO (a, AppState)
run app maxdepthtogo = runStateT (runReaderT app config) initState
    where
          config = AppConfig maxdepthtogo
          initState = AppState 0

depthLimitedCountEntries :: FilePath -> Int -> App [(FilePath, Int)]
depthLimitedCountEntries path currDepth = do
    appConfig <- ask
    filedirs <- liftIO $ listFileDirs path
    y <- liftM concat $ forM filedirs (processFileOrDir appConfig)
    liftIO $ return $ (path, length filedirs) : y

    where
        isDir :: FilePath -> App Bool
        isDir x = liftIO $ (return . searchable) =<< getPermissions x

        processFileOrDir :: AppConfig -> FilePath -> App [(FilePath, Int)]
        processFileOrDir appConfig fileOrDir = do
        let fullPath = path </> fileOrDir
            cfgMaxDepth = maxDepthToGo appConfig
        isDir fullPath >>= \isDirectory ->
            if isDirectory && (currDepth < cfgMaxDepth)
                then do
                    curState <- get
                    let newDepth = currDepth + 1
                    when (currDepth < cfgMaxDepth) $ put curState{getMaxDepthReached = newDepth} -- update the state
                    depthLimitedCountEntries fullPath newDepth
                else return []

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
    filedirs <- liftIO $ listFileDirs path
    y <- liftIO $ liftM concat $ forM filedirs processFileOrDir
    tell $ (path, length filedirs) : y

    where
        isDir :: FilePath -> IO Bool
        isDir x = (return . searchable) =<< getPermissions x

        processFileOrDir :: FilePath -> IO [(FilePath, Int)]
        processFileOrDir fileOrDir = do
        let fullPath = path </> fileOrDir
        isDir fullPath >>= \isDirectory ->
            if isDirectory
                then execWriterT $ countEntries fullPath
                else return []

main :: IO [(FilePath, Int)]
main = do
    execWriterT $ countEntries ".."
    liftM fst $ run (depthLimitedCountEntries ".." 0) 1 -- 0 is the starting depth, 1 is the deepest we want to go

listFileDirs :: FilePath -> IO [FilePath]
listFileDirs path = liftM (filter f) $ getDirectoryContents path
    where f :: FilePath -> Bool
          f = flip notElem [".",".."]
