module Main where

import Criterion.Main
import qualified Data.Sequence as S
import Data.Sequence ((|>))

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push x q = Queue (x : enqueue q) (dequeue q)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue xs []) = pop $ Queue [] (reverse xs)
pop (Queue xs (y:ys)) = Just (y, Queue xs ys)

data Q a =
  Q { getL :: [a] } deriving (Eq, Show)

enq :: a -> Q a -> Q a
enq x q = Q (getL q ++ [x])

deq :: Q a -> Maybe (a, Q a)
deq q = case getL q of
  x:xs -> Just (x, Q xs)
  _ -> Nothing

manipulate :: Queue Int -> Queue Int
manipulate q =
  case pop q of
    Nothing -> push 1 (Queue [] [])
    Just (_, q') -> push 1 q'

manipulate' :: Q Int -> Q Int
manipulate' q =
  case deq q of
    Nothing -> enq 1 (Q [])
    Just (_, q') -> enq 1 q'

manipulate'' :: S.Seq Int -> S.Seq Int
manipulate'' s = S.drop 1 s |> 1

main :: IO ()
main = defaultMain
  [ bench "queue based on two lists" $
    whnf (foldr (.) id (replicate 1000 manipulate)) (Queue [100,99..51] [1..50])
  , bench "queue based on a single list" $
    whnf (foldr (.) id (replicate 1000 manipulate')) (Q [1..100])
  , bench "seq" $
    whnf (foldr (.) id (replicate 1000 manipulate'')) (S.fromList [1..100])
  ]
