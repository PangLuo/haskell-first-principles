module MaybeT where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype MaybeT m a
  = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Functor m, Monad m) => Applicative (MaybeT m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (MaybeT m) where
  fail _ = MaybeT (return Nothing)
  return = lift . return
  x >>= f = MaybeT $ do
    v <- runMaybeT x
    case v of
      Nothing -> return Nothing
      Just y  -> runMaybeT (f y)

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance (MonadIO m)
      => MonadIO (MaybeT m) where
  liftIO = lift . liftIO
