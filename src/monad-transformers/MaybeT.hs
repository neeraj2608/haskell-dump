module MaybeT where

import Control.Monad.Trans
import Control.Monad

main :: IO ()
main = undefined

newtype MaybeT m a = MaybeT {runMaybe :: m (Maybe a)}

instance Monad m => Monad (MaybeT m) where
    return a = MaybeT $ return $ Just a
    (>>=) g h = MaybeT $ runMaybe g >>= maybe (return Nothing) (runMaybe . h)
    fail _ = MaybeT $ return Nothing

instance MonadTrans MaybeT where -- lift any monad into MonadTrans
    lift m = MaybeT (liftM Just m)
