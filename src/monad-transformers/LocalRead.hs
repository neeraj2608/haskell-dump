module LocalRead where

import Control.Monad.Reader

-- testing normal Reader
testReader :: Reader String String
testReader = do
    text <- ask
    return ("hi " ++ text)

-- testing local
otherReader :: Reader String String
otherReader = local ("hello " ++) testReader -- note that the "hello" ++ bit acts on the "there" that we pass in below i.e.,
                                             -- the 'r' in Reader r m

-- testing ReaderT
anotherReader :: ReaderT String IO ()
anotherReader = do
    text <- ask
    liftIO $ putStrLn ("hi " ++ text)

main :: IO ()
main = do
        putStrLn $ runReader testReader "there"
        putStrLn $ runReader otherReader "there"
        runReaderT anotherReader "there" -- already returns IO (). No need for putStrLn
