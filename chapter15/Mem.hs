module Mem where

import Test.QuickCheck

newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem (\s ->
    let (a, s') = f s
        (a', s'') = g s'
    in (a <> a', s''))

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)

instance (Arbitrary s, CoArbitrary s, Arbitrary a) =>
         Arbitrary (Mem s a) where
  arbitrary = do a <- arbitrary
                 f <- arbitrary
                 return $ Mem $ \s -> (a, f s)

instance Show (Mem s a) where
  show _ = "Mem"

prop_assoc :: Mem Int String
           -> Mem Int String
           -> Mem Int String
           -> Int
           -> Bool
prop_assoc f g h s = runMem (f <> g <> h) s == runMem ((f <> g) <> h) s

prop_id :: Mem Int String -> Int -> Bool
prop_id f s = runMem (mempty <> f) s == runMem f s &&
              runMem (f <> mempty) s == runMem f s

f' :: Num s => Mem s String
f' = Mem $ \s -> ("hi", s + 1)

main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print rmleft
  print rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

  putStrLn "associativity check"
  quickCheck prop_assoc
  putStrLn "identity check"
  quickCheck prop_id
