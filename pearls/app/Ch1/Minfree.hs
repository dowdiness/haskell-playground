-- Arrayを使った最小自由数の求め方
-- 順序集合の最小要素を求めるときに使える？
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Minfree (main) where

import Data.Array
import Data.List (partition)

main :: IO ()
main = print $ minfree' [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]

minfree :: [Int] -> Int
minfree = search . checklist

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) (map (, True) (filter (>= 0) xs))
    where n = maximum xs

-- zip を使うような操作はTuple sectionsを使って解ける
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/tuple_sections.html
-- checklist xs = accumArray (||) False (0, n) (zip (filter (>= 0) xs) (repeat True))

countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0, n) (map (, 1) (filter (>= 0) xs))
    where n = maximum xs

-- 分割統治法を使った最小自由数の求め方

minfree' :: [Int] -> Int
minfree' xs = minfrom 0 (length xs, xs)

minfrom :: Int -> (Int, [Int]) -> Int
minfrom a (n, xs)
    | n == 0     = a
    | m == b - a = minfrom b (n - m, vs)
    | otherwise  = minfrom a (m, us)
    where
        (us, vs) = partition (<b) xs
        b        = a + 1 + n `div` 2
        m        = length us
