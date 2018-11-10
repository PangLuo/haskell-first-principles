{-# LANGUAGE FlexibleInstances #-}

module Flip where

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b = K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K a) = K a

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)
