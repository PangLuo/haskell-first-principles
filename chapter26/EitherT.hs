module EitherT where

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m
      => Functor (EitherT e m) where
  fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance Applicative m
      => Applicative (EitherT e m) where
  pure x = EitherT (pure (pure x))

  (EitherT fab) <*> (EitherT mma) =
    EitherT $ (<*>) <$> fab <*> mma

instance Monad m
      => Monad (EitherT e m) where
  return = pure

  (EitherT ma) >>= f =
    EitherT $ do
      v <- ma
      case v of
        Left l -> return $ Left l
        Right r -> runEitherT $ f r

swapEither :: Either e a -> Either a e
swapEither (Left l) = Right l
swapEither (Right r) = Left r

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ swapEither <$> ma

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT mab) = do
  v <- mab
  case v of
    Left l -> f l
    Right r -> g r
