{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module OpenTF where

import GHC.Types 

data Blah a 
data Either a b = L a | R b
  
type family Elem a :: Type
type instance Elem (Blah a) = a

type family HTF a :: Type -> Type
type instance HTF (Blah a) = Either a

-- foobar :: Elem [a] -> a
-- foobar = undefined

-- barfoo :: HTF (Blah a) b -> Either a b
-- barfoo = \x -> x
