{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE NoPartialTypeConstructors #-}
#endif
module GHC.Unit.Types where

import GHC.Prelude ()
import {-# SOURCE #-} GHC.Utils.Outputable
import {-# SOURCE #-} GHC.Unit.Module.Name ( ModuleName )
import Data.Kind (Type)

data UnitId
data GenModule (unit :: Type)
data GenUnit (uid :: Type)

type Module      = GenModule  Unit
type Unit        = GenUnit    UnitId

moduleName :: GenModule a -> ModuleName
moduleUnit :: GenModule a -> a
pprModule :: Module -> SDoc
