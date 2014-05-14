module Transform where
{-
- Using monad transformers
-}

import System.FilePath
import System.Directory
import Control.Monad

listFileDirs :: FilePath -> IO [FilePath]
listFileDirs path = liftM (filter f) $ getDirectoryContents path
    where f :: FilePath -> Bool
          f = flip notElem [".",".."]

countEntries :: FilePath -> IO [(FilePath, Int)]
countEntries path = do
    filedirs <- listFileDirs path
    y <- liftM concat $ forM filedirs processFileOrDir
    return $ (path, length filedirs) : y

    where
        isDir :: FilePath -> IO Bool
        isDir x = (return . searchable) =<< getPermissions x

        processFileOrDir :: FilePath -> IO [(FilePath, Int)]
        processFileOrDir fileOrDir = do
        let fullPath = path </> fileOrDir
        isDir fullPath >>= \isDirectory ->
            if isDirectory
                then countEntries fullPath
                else return []

main :: IO [(FilePath, Int)]
main = countEntries ".."
