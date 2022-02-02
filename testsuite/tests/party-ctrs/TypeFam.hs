{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}

module TypeFam where

import GHC.Types (Total, type (@))

type That f a b = a -> f b
type ThisFam a = forall f. (Applicative f, Total f) => That f a a
type ThisNorm a = forall f. (Applicative f) => That f a a

data D = D1 String | D3 Int
  deriving Eq

thisi :: ThisFam Int
thisi = pure

thisb :: ThisFam Bool
thisb = pure

this' :: a -> ThisFam b -> ThisFam Int
this' a b = pure


blah :: D -> ThisFam Int
blah d = case d of
           D1 s -> this' s thisi
           D3 i -> this' i thisb
