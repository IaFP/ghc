{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module TreeTF1 where

import GHC.Types (type (@@), Constraint, Type)

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

type family F (a :: Type) :: Type
type instance F [a] = Tree a

-- Explicit elaboration. The intent
-- is to do this under the hood.
type family F_ (a :: Type) :: Constraint
type instance F_ [a] = Tree @@ a

-- now F_ [a] ~ Tree @@ a ~ Ord a,
-- so this is fine. Want to put F_ [a] in
-- constraints of g.
g :: F_ [a] => a -> F [a]
g = Leaf
