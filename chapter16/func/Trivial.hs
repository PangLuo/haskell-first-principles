module Trivial where

data Trivial = Trivial

-- We can't implement a Functor instance for this type because Trivial is
-- a type constant.
