module MakeBottom where

x = undefined
y = "blah"
main = do
  print $ x `seq` (snd (x, y))
