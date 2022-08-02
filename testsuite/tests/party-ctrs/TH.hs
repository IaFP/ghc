{-# LANGUAGE InstanceSigs, KindSignatures
    , GADTs, RankNTypes, ConstraintKinds, ScopedTypeVariables
    , FlexibleInstances, ExplicitNamespaces, TypeOperators
    , TemplateHaskell #-}

module TH where

import GHC.Exts (Constraint)

import Control.Applicative
import Control.Monad
import GHC.Types (type (@))
-------------------------------------------------------------------------------------------------

import Language.Haskell.TH.Syntax

oneC, twoC, plusC :: Quote m => m Exp
oneC = [| 1 |]
twoC = [| 2 |]
plusC = [| $oneC + $twoC |]
