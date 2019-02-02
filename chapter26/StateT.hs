module StateT where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance (Functor m)
      => Functor (StateT s m) where
  fmap f (StateT sma) =
    StateT $ \s -> fmap f' $ sma s
      where f' (a, s) = (f a, s)

instance (Monad m)
      => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)

  (StateT fmab) <*> (StateT sma) =
    StateT $ \s -> do
      (f, s') <- fmab s
      (a, s'') <- sma s'
      return (f a, s'')

instance (Monad m)
      => Monad (StateT s m) where
  return = pure

  (StateT sma) >>= f =
    StateT $ \s -> do
      (a, s') <- sma s
      runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)

instance (MonadIO m)
      => MonadIO (StateT s m) where
  liftIO = lift . liftIO
