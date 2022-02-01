{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module OpenTF where

import GHC.Types (Type)


data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

type instance Elem (Tree a) = a


