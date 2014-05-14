module Transform where
{-
- Using monad transformers
-}

import System.FilePath
import System.Directory
import Control.Monad
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Trans (liftIO)

listFileDirs :: FilePath -> IO [FilePath]
listFileDirs path = liftM (filter f) $ getDirectoryContents path
    where f :: FilePath -> Bool
          f = flip notElem [".",".."]

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
main = execWriterT $ countEntries ".."
