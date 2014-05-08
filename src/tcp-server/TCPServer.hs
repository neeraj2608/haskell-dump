{-
-- Super basic TCP Server
-}
module TCPServer where

import Network
import Control.Concurrent (forkIO)
import System.Environment (getArgs)
import System.IO (Handle, hSetBuffering, BufferMode(..))
import Data.Text.IO (hGetLine)
import Data.Text (Text, unpack)
import Control.Monad (forever)
import Control.Exception (SomeException(..), handle)

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    if null args
       then error "portID number is required"
       else do
           let portID = PortNumber $ fromIntegral ((read $ head args) :: Int)
           putStrLn $ "Starting server on portID "++ show portID
           socket <- listenOn portID
           socketHandler =<< accept socket

socketHandler :: (Handle, HostName, PortNumber) -> IO ()
socketHandler x@(h, _, _) = do
    hSetBuffering h NoBuffering
    handle (\(SomeException _) -> putStrLn "Client disconnected")
           (hGetLine h >>= (forkIO . processText) >> socketHandler x)

processText :: Text -> IO ()
processText x = putStrLn $ processInput $ (words . unpack) x

processInput :: [String] -> String
processInput (y:ys) = case y of
    "echo" -> unwords ys
    _ -> "Unknown command " ++ y
processInput _ = ""