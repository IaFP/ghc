{-# LANGUAGE CPP #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGAUGE ExplicitNamespaces #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, QuantifiedConstraints #-}

module BstADT where

import GHC.Types (type (@))

data Ord a => BST a = Leaf | Node a (BST a) (BST a)
newtype Ord a => OList a = OList [a]

deriving instance Show a => Show (BST a)

insert'bst :: a -> BST a -> BST a
insert'bst v Leaf = Node v Leaf Leaf
insert'bst v n@(Node h l r) =
  if (v == h)
  then n
  else if (v <= h)
       then Node h (insert'bst v l) r
       else Node h l (insert'bst v r)

flatten'bst :: BST a -> [a]
flatten'bst Leaf = []
flatten'bst (Node h l r) = (flatten'bst l) ++ [h] ++ (flatten'bst r)

unflatten'bst :: [a] -> BST a
unflatten'bst []  =  Leaf
unflatten'bst lst = (Node h l r)
  where
    pvt :: Int
    pvt = (length lst) `div` 2
    fs = splitAt pvt lst
    l :: BST a
    l = unflatten'bst (fst fs)
    r :: BST a
    r = unflatten'bst $ init (snd fs)
    h :: a
    h = head (fst fs)

merge :: BST a -> BST a -> BST a
merge t t' = unflatten'bst $ msort (flatten'bst t) (flatten'bst t')

msort :: Ord a => [a] -> [a] ->[a]
msort [] a = a
msort a [] = a
msort (l:l') (r:r') = 
  if (l <= r)
  then l : (msort l' (r:r'))
  else r : (msort (l:l') r')

fmap'bst :: (a -> b) -> BST a -> BST b
fmap'bst f Leaf = Leaf
fmap'bst f (Node d lt rt) = insert'bst (f d) $ merge (fmap'bst f lt) (fmap'bst f rt)

instance Functor BST where
    fmap = fmap'bst

bst1 :: BST Int
bst1 = insert'bst (1::Int) (Leaf)

emptyBST :: BST a
emptyBST = Leaf -- Just a top level term declaration will give rise to ambiguous type error. (Why?)

mkBST :: [a] -> BST a
mkBST = foldr insert'bst Leaf

-- bst2 :: BST Int
bst2 = mkBST [1, 2, 3, 4]
