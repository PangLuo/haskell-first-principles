module Lift where

import Control.Monad
import Control.Monad.Trans.Class

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)
