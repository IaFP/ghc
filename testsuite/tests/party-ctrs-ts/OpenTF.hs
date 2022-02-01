{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module TreeTF2 where

import GHC.Types (Type)

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

type family F a

-- F is left open -- we don't know what it will elaborate to
g :: a -> F [a]
g = undefined
