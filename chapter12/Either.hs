module Either where

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Left a) xs = a:xs
        f _ xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Right a) xs = a:xs
        f _ xs = xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
  where f (Left a) t = (a:fst t, snd t)
        f (Right b) t = (fst t, b:snd t)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' _ _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' g = either' (const Nothing) (Just . g)
