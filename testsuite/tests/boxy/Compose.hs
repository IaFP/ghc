{-# OPTIONS_GHC -XImpredicativeTypes -fno-warn-deprecated-flags -XEmptyDataDecls -XGADTs #-}

-- #1118

module Compose where
import GHC.Types (type (@@), Total)
data Z
data S n

data List n a where
    Nil :: List Z a
    (:-) :: a -> List n a -> List (S n) a

data Total a => Hold a = Hold (forall m. a m -> a (S m))

compose' :: List n (Hold a) -> a (S Z) -> a (S n)
compose' Nil x = x
compose' ((Hold f) :- fs) x = f (compose' fs x)

compose :: Total a => List n (forall m . a m -> a (S m)) -> a (S Z) -> a (S n)
compose Nil x = x
compose (f :- fs) x = f c
  where c = (compose fs x)

composeS :: Total a => [forall m . a m -> a m] -> a n -> a n
composeS [] x = x
composeS (f:fs) x = f (composeS fs x)
