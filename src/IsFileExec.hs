module IsFileExec where

import System.IO as IO
import Control.Monad
import System.Environment
import Data.ByteString.Lazy as L

-- ByteStrings
isExecutable :: FilePath -> IO Bool
isExecutable x = do
                   contents <- L.readFile x
                   return $ hasElfMagic contents

hasElfMagic :: ByteString -> Bool
hasElfMagic input = L.take 4 input == L.pack [0x7f, 0x45, 0x46, 0x4c]

main :: IO ()
main = do
         filePath <- getArgs
         x <- liftM (\x -> if x then "True" else "False") $ isExecutable $ Prelude.head filePath
         IO.putStrLn x
