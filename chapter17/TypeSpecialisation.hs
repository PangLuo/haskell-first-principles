module TypeSpecialisation where

listPure :: a -> [] a
listPure = pure

listApply :: [] (a -> b) -> [] a -> [] b
listApply = (<*>)

ioPure :: a -> IO a
ioPure = pure

ioApply :: IO (a -> b) -> IO a -> IO b
ioApply = (<*>)

tuplePure :: Monoid a => a -> (a, a)
tuplePure = pure

tupleApply :: Monoid a => (a, a -> b) -> (a, a) -> (a, b)
tupleApply = (<*>)

funPure :: a -> e -> a
funPure = pure

funApply :: (e -> a -> b) -> (e -> a) -> e -> b
funApply = (<*>)
