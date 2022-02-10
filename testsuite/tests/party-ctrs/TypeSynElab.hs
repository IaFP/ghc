{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators, RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module TypeSynElab where

import GHC.Types (Total, type (@))

type That f a b = a -> f b
type This a = forall f. (Applicative f, Total f) => That f a a

data PA a = PA a

data D = D1 | D2 | D3

type Blah a = That PA a a

wr :: Blah a -> Blah a -> Blah a
wr l1 l2 = undefined
{-# INLINE wr #-}

x :: Blah a -> Blah a
x = (wr y)

y :: Blah a
y = undefined

data T a b c = T a b c
type Syn a = T a

type App f a = Ap (f a)
newtype Ap p = Ap p 
