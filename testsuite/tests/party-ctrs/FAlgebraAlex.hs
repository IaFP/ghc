{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeConstructors, ExplicitNamespaces, TypeOperators, ScopedTypeVariables #-}

module FAlgebraAlex where

import GHC.Types (type (@@))

newtype Mu f = In { out :: (f (Mu f)) }

newtype  McataAlg f x = McataAlg (forall r . (f @@ r) => (r -> x) -> f r -> x) 
mcata :: McataAlg f x -> Mu f -> x
mcata (McataAlg alg) (In d) = alg (mcata (McataAlg alg)) d

newtype Functor f =>  MhistoAlg f x = MhistoAlg (forall r. (f @@ r) => (r -> f r) -> (r -> x) -> f r -> x)
mhisto :: MhistoAlg f x -> Mu f -> x
mhisto (MhistoAlg alg) (In d) = alg out (mhisto (MhistoAlg alg)) d

data FooF r = Noo | Coo (r -> r) r

type Foo = Mu FooF

noo = In Noo
coo f xs = In (Coo f xs)


lenFoo :: Foo -> Int
lenFoo = mcata (McataAlg alg) where
  alg _ Noo = 0
  alg len (Coo f xs) = 1 + len (f xs)

-- This is ill-typed as its non-terminating
loopFoo :: Foo -> Int
loopFoo = mhisto (MhistoAlg alg) where
   -- alg :: MhistoAlg FooF Int
   alg _ _ Noo = 0
   alg out' eval (Coo f xs) = case (out' xs) of
                               Noo        -> 1 + eval (f xs)
                               (Coo f' _) -> 1 + eval (f' xs)

