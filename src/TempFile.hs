{-
- Procesing with temp files
-}
import System.IO
import System.Directory (getTemporaryDirectory,removeFile) 
import Control.Exception (catch, finally, IOException)

main :: IO ()
main = withTempFile processFile

withTempFile :: (Handle -> IO()) -> IO () 
withTempFile processFile = do
                             fileDir <- catch (getTemporaryDirectory)
                                                ((\_ -> return ".")::IOException->IO FilePath) -- note how the exception type of the lambda needs to be specified
                             (tempFileName, tempFileHandle) <- openTempFile fileDir "tempFile.txt"
                             putStrLn $ "Using file " ++ show tempFileName
                             finally (do
                                        processFile tempFileHandle
                                        putStrLn "Using writeFile"
                                        writeFile tempFileName (show [1..10])
                                        putStrLn "Using readFile"
                                        contents <- readFile tempFileName
                                        putStrLn contents)
                                     (do
                                        hClose tempFileHandle
                                        putStrLn $ "Deleting file " ++ tempFileName
                                        removeFile tempFileName)

processFile :: Handle -> IO ()
processFile fileHandle = do
                           let tempData = show ([1..10]::[Int]) -- stuff to write to file
                           pos <- hTell fileHandle
                           putStrLn $ "Current position in file " ++ show pos
                           hPutStrLn fileHandle tempData
                           pos <- hTell fileHandle
                           putStrLn $ "Current position in file " ++ show pos
                           hSeek fileHandle AbsoluteSeek 0
                           c <- hGetContents fileHandle
                           putStrLn "File contains:"
                           putStrLn c
