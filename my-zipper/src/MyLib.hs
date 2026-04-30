module MyLib (output, go) where

import Data.Function

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

data Direction = L | R deriving (Show)

type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L : ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R : ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r
changeToP _ Empty = Empty

elemAt :: Directions -> Tree a -> a
elemAt (L : ds) (Node _ l _) = elemAt ds l
elemAt (R : ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x
elemAt _ Empty = error "Empty tree"

freeTree :: Tree Char
freeTree =
  Node
    'P'
    ( Node
        'O'
        ( Node
            'L'
            (Node 'N' Empty Empty)
            (Node 'T' Empty Empty)
        )
        ( Node
            'Y'
            (Node 'S' Empty Empty)
            (Node 'A' Empty Empty)
        )
    )
    ( Node
        'L'
        ( Node
            'W'
            (Node 'C' Empty Empty)
            (Node 'R' Empty Empty)
        )
        ( Node
            'A'
            (Node 'A' Empty Empty)
            (Node 'C' Empty Empty)
        )
    )

output :: IO ()
output = print (elemAt [R, L] newTree)
  where
    newTree = changeToP [R, L] freeTree

data Crumb a
  = LeftCrumb a (Tree a)
  | RightCrumb a (Tree a)
  deriving (Show)

type Breadcrums a = [Crumb a]

type Zipper a = (Tree a, Breadcrums a)

goLeft :: Zipper a -> Zipper a
goLeft (Node x l _, bs) = (l, LeftCrumb x l : bs)
goLeft (Empty, bs) = (Empty, bs)

goRight :: Zipper a -> Zipper a
goRight (Node x _ r, bs) = (r, RightCrumb x r : bs)
goRight (Empty, bs) = (Empty, bs)

go :: IO ()
go = print $ (freeTree, []) & goRight & goLeft
