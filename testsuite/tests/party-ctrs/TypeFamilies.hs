{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies, RankNTypes #-}


module TypeFamilies where

import GHC.Types
-- data Ord a => BST a = Tip | Branch a (BST a) (BST a)

data Blah a 
data Either a b = L a | R b
  
type family NTF a :: Type
type instance NTF (Blah a) = a

type family HTF a :: Type -> Type
type instance HTF (Blah a) = Either a

-- foobar :: Elem [a] -> a
-- foobar = undefined

-- barfoo :: HTF (Blah a) b -> Either a b
-- barfoo = \x -> x
