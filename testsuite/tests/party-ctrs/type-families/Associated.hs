{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module Associated where

class Collection a where
  type family Elem a
  e :: a
  cons :: Elem a -> a -> a


instance Collection [a] where
  type instance Elem [a] = a
  e = []
  cons = (:)

type Foobar a = [a]

consEmpty :: Elem a -> a
consEmpty hd = undefined
