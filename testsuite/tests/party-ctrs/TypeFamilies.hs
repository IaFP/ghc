{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies, RankNTypes #-}


module TypeFamilies where


data Ord a => BST a = Tip | Branch a (BST a) (BST a)

type family F a :: *
type instance F a = BST a 

f :: a -> F a -> F a   -- this is fine as F ~> BST and BST Int is well defined
f = insert'bst

insert'bst :: a -> BST a -> BST a
insert'bst v Tip = Branch v Tip Tip
insert'bst v n@(Branch h l r)
  | v == h = n
  | v < h = Branch h (insert'bst v l) r
  | otherwise  = Branch h l (insert'bst v r)

-- fail_bst = f id Tip -- This should fail

ok_bst = f 3 Tip
