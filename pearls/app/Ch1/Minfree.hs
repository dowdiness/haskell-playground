-- Arrayを使った最小自由数の求め方
-- 順序集合の最小要素を求めるときに使える？

module Minfree (main) where

import Data.Array
import Data.List (partition)
import Operators ((\\))

main :: IO ()
main = print $ minfree3 [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]

minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)

minfree2 :: [Int] -> Int
minfree2 = search . checklist

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

-- Haskell の Array を使ったアルゴリズムの実装例
-- https://zenn.dev/naoya_ito/articles/87a8a21d52c302
-- 関数適用による状態遷移で DP を解く
-- https://publish.obsidian.md/naoya/articles/%E9%96%A2%E6%95%B0%E9%81%A9%E7%94%A8%E3%81%AB%E3%82%88%E3%82%8B%E7%8A%B6%E6%85%8B%E9%81%B7%E7%A7%BB%E3%81%A7+DP+%E3%82%92%E8%A7%A3%E3%81%8F

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

minfree3 :: [Int] -> Int
minfree3 xs = minfrom 0 (length xs, xs)

minfrom :: Int -> (Int, [Int]) -> Int
minfrom a (n, xs)
    | n == 0     = a
    | m == b - a = minfrom b (n - m, vs)
    | otherwise  = minfrom a (m, us)
    where
        (us, vs) = partition (<b) xs
        b        = a + 1 + n `div` 2
        m        = length us
