module Maybe where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee b _ _ = b

fromMaybe :: a -> Maybe a -> a
fromMaybe = flip mayybee id

listToMaybe :: [a] -> Maybe a
listToMaybe (x:_) = Just x
listToMaybe _ = Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList _ = []

catMaybes :: [Maybe a] -> [a]
catMaybes (Just x : xs) = x : catMaybes xs
catMaybes (_:xs) = catMaybes xs
catMaybes _ = []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr (\x xs ->
  case (x, xs) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just x', Just xs') -> Just $ x':xs')
  (Just [])

