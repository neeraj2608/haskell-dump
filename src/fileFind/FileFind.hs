{-
- File finding
-}
module FileFind where
 
import System.IO
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad

-- strict find
listFiles :: FilePath -> IO [FilePath]
listFiles currentDir = do
  filesDirs <- liftM removeCurrentAndParentDir $ getDirectoryContents currentDir
  results <- liftM concat $ mapM recurseListFiles filesDirs
  filterM notEmpty results
  where
    removeCurrentAndParentDir :: [FilePath] -> [FilePath]
    removeCurrentAndParentDir = filter (`notElem` [".",".."])

    recurseListFiles :: FilePath -> IO [FilePath]
    recurseListFiles y = do
      let fullPath = currentDir </> y
      isDirectory <- doesDirectoryExist fullPath
      if isDirectory
        then listFiles fullPath
        else return [fullPath]

    notEmpty :: FilePath -> IO Bool
    notEmpty = return . not . null
