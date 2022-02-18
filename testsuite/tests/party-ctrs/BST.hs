{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeConstructors, ExplicitNamespaces, TypeOperators, UndecidableInstances, StandaloneDeriving #-}

module BST where

import GHC.Types (type (@))

-- data (Ord a) => BST a where
--   Leaf :: BST a
--   Node :: a -> BST a -> BST a -> BST a
  
newtype Ord a => BST a = BST { unBST :: Tree a }
                       deriving Show
-- instance (Show a) => Show (BST a) where
--   show 

-- deriving instance (Show a, Ord a) => Show (BST a)
insert'bst :: a -> BST a -> BST a
insert'bst v bst = BST (insert v (unBST bst))

flatten'bst :: BST a -> [a]
flatten'bst = flatten . unBST

unflatten'bst :: [a]-> BST a
unflatten'bst = BST . unflatten

merge'bst :: BST a -> BST a -> BST a
merge'bst t t' = BST (merge (unBST t) (unBST t'))

empty'bst :: BST a
empty'bst = BST Leaf

fmap'bst :: (a -> b) -> BST a -> BST b
fmap'bst f bst
  | Leaf <- unBST bst = BST Leaf
  | (Node e lt rt) <- unBST bst = insert'bst (f e) $ merge'bst (fmap f (BST lt)) (fmap f (BST rt))

instance Functor BST where
   fmap = fmap'bst

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

insert :: Ord a => a -> Tree a -> Tree a
insert v Leaf = Node v Leaf Leaf
insert v n@(Node h l r)
  | v == h = n
  | v < h = Node h (insert v l) r
  | otherwise  = Node h l (insert v r)


flatten :: Ord a => Tree a -> [a]
flatten Leaf = []
flatten (Node h l r) = (flatten l) ++ [h] ++ (flatten r)

unflatten :: [a] -> Tree a
unflatten []  =  Leaf
unflatten lst = (Node h l r)
  where pvt = (length lst) `div` 2
        (f, s) = splitAt pvt lst
        l = unflatten f
        r = unflatten $ init s
        h = head s

merge :: Ord a => Tree a -> Tree a -> Tree a 
merge t t' = unflatten $ msort (flatten t) (flatten t')

msort :: (Ord a) => [a] -> [a] ->[a]
msort [] a = a
msort a [] = a
msort (l:l') (r:r')
  | l <= r = l : (msort l' (r:r'))
  | otherwise = r : (msort (l:l') r')
