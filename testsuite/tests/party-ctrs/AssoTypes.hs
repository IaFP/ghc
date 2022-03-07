{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module AssoTypes where

import GHC.Types


class Eq (Associated a) => Foo a where
    type Associated a :: Type
    foo :: a -> Associated a -> Bool

instance Foo () where
    type Associated () = Int
    foo _ x = x == x

data Tree a = L a | B a (T a) (T a)

class Bar a where
  type BarTy a :: Type -> Type
  bar :: a -> BarTy a

instance Bar (M a) where
  type BarTy a = Tree
  bar a = L a 
