{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module OpenTFImporter where

import OpenTF

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

type instance Elem (Tree a) = a

blahblah :: Elem a -> a
blahblah = undefined
