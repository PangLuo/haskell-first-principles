module MyLiftA2 where

myLiftA2 :: Applicative f =>
            (a -> b -> c) -> f a -> f b -> f c
myLiftA2 h fa fb = h <$> fa <*> fb
