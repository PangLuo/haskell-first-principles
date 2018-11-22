module Ask where

newtype Reader r a =
  Reader { runReader :: r -> a }

-- The type a can be anything in the universe. It has infinite inhabitants.
ask :: Reader a a
ask = Reader id
