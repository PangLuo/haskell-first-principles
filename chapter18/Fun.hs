module Fun where

import Control.Monad

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m
   => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m
    => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) k = (:) <$> (k a) <*> (meh as k)

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id
