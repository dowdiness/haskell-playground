-- https://pfad.sampou.org

module Operators (
    (\\)
  , (∈), (∉)
  , (∨), (∧)
  , (≤), (≥)
) where

import qualified Data.List

infixr 5 \\

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = (Data.List.\\)

infix 4 ∈, ∉

(∈) :: Eq a => a -> [a] -> Bool
(∈) = elem

(∉) :: Eq a => a -> [a] -> Bool
(∉) = notElem

infixr 2 ∨

(∨) :: Bool -> Bool -> Bool
(∨) = (||)

infixr 3 ∧

(∧) :: Bool -> Bool -> Bool
(∧) = (&&)

infix 4 ≤, ≥

(≤) :: Ord a => a -> a -> Bool
(≤) = (<=)

(≥) :: Ord a => a -> a -> Bool
(≥) = (>=)
