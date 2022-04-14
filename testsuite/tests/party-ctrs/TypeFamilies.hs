{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies, RankNTypes #-}


module TypeFamilies where

import GHC.Types

data Blah a 
data Either a b = L a | R b
type TSyn a = forall b. Either a b -> Blah a
  
type family NTF a :: Type
type instance NTF (Blah a) = a

type family HTF a :: Type -> Type
type instance HTF (Blah a) = Either a

-- type family TTF a :: Type
-- type instance TTF (TSyn a) = a -- Error: illegal polymorphic type forall b. Either a b -> Blah a
