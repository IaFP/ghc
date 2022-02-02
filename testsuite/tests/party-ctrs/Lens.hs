{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators, RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Lens where

import GHC.Types (Total, type (@))

type LensLike  f s t a b = (a -> f b) -> s -> f t

type Traversal s t a b = forall f. Applicative f => LensLike f s t a b

type Traversal' s a = Traversal s s a a


data PD = PD1 | PD2
data Lib = Lib1 | Lib2
data BI = BI1 | BI2

allLibraries :: Traversal' PD Lib
allLibraries = undefined

libName :: Traversal' Lib BI
libName = undefined


componentBuildInfo' :: Traversal' PD a -> Traversal' a BI  -> Traversal' PD BI
componentBuildInfo' a b = undefined

componentBuildInfo :: Traversal' PD BI
componentBuildInfo = componentBuildInfo' allLibraries libName
