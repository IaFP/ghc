{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}


module TypeFamilies where


data Ord a => BST a = Tip | Branch a (BST a) (BST a)

type family F a :: *
type instance F a = BST a 

f :: a -> F a -> F a   -- No implicit Ord constraint here!
f = insert'bst

insert'bst :: a -> BST a -> BST a
insert'bst v Tip = Branch v Leaf Leaf
insert'bst v n@(Branch h l r)
  | v == h = n
  | v < h = Branch h (insert'bst v l) r
  | otherwise  = Branch h l (insert'bst v r)
