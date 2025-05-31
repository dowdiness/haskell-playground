module Surpasser (main) where

import Data.List (tails)

main :: IO ()
main = print $ msc ("GENERATING" :: String)

-- Maximum Surpasser Count
-- Quadratic time implementation
-- O(n^2) time and O(1) space
-- This function counts the number of elements that are greater than each element in the list.
-- It returns the maximum count found, or 0 if the list is empty.
-- Example: msc [3, 1, 4, 2] returns 2 because:
-- 3 has 2 elements (4, 2) that are greater than it.
msc :: Ord a => [a] -> Int
msc xs = maximum (0 : [ scount z zs | (z:zs) <- tails xs ])

scount :: Ord a => a -> [a] -> Int
scount x xs = length $ filter (x<) xs
