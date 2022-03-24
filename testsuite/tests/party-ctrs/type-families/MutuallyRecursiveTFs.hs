{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}

module MutuallyRecursiveTFs where

{-
  This is quite dumb, but if it fails it may highlight
  an error in group checking.
-}

type family G a
type instance G a = F a
  
type family F a
type instance F a = G a

{--------------------------------------------------------------------------------

  I suspect it will be more malicious in closed TFs,
  as a type family's TyCon and instances are defined
  simultaneously.
-}

type family G' a where
  G' a = F' a

type family F' a where
  F' a = G' a

