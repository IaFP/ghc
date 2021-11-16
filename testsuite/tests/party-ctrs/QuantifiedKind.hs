{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}

module QuantifiedKind where

import Data.Kind
import GHC.Types (Constraint)

data T (f :: forall a. a -> Type) = MkT (f Bool)

