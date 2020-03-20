{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, UndecidableInstances, StandaloneDeriving #-}

module BST where

import GHC.Types (type (@@))

data (Ord a) => BST a where
  Leaf :: BST a
  Node :: a -> BST a -> BST a -> BST a

-- instance (Show a{-, BST @@ a-}) => Show (BST a) where
--   show Leaf = "Leaf"
--   show (Node a lt rt) = "Node" ++ show a ++ (show lt) ++ (show rt)

deriving instance Show a => Show (BST a)

insert'bst :: a -> BST a -> BST a
insert'bst v Leaf = Node v Leaf Leaf
insert'bst v n@(Node h l r)
  | v == h = n
  | v < h = Node h (insert'bst v l) r
  | otherwise  = Node h l (insert'bst v r)

flatten'bst :: BST a -> [a]
flatten'bst Leaf = []
flatten'bst (Node h l r) = (flatten'bst l) ++ [h] ++ (flatten'bst r)

unflatten'bst :: [a] -> BST a
unflatten'bst []  =  Leaf
unflatten'bst lst = (Node h l r)
  where pvt = (length lst) `div` 2
        (f, s) = splitAt pvt lst
        l = unflatten'bst f
        r = unflatten'bst $ init s
        h = head s
        
merge :: BST a -> BST a -> BST a
merge t t' = unflatten'bst $ msort (flatten'bst t) (flatten'bst t')

msort :: (Ord a) => [a] -> [a] ->[a]
msort [] a = a
msort a [] = a
msort (l:l') (r:r')
  | l <= r = l : (msort l' (r:r'))
  | otherwise = r : (msort (l:l') r')

fmap'bst :: (a -> b) -> BST a -> BST b
fmap'bst f Leaf = Leaf
fmap'bst f (Node d lt rt) = insert'bst (f d) $ merge (fmap'bst f lt) (fmap'bst f rt)

instance Functor BST where
   fmap = fmap'bst

