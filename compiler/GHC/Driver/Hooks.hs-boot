{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE ExplicitNamespaces, TypeOperators, UndecidableSuperClasses, TypeFamilies #-}
#endif
module GHC.Driver.Hooks where

import GHC.Prelude ()
#if MIN_VERSION_base(4,16,0)
import GHC.Types (type(@), WDT)
import qualified Data.Kind
#endif

#if MIN_VERSION_base(4,16,0)
-- See Note [The Decoupling Abstract Data Hack]
type family DsForeignsHook :: Data.Kind.Type
#endif

data Hooks

emptyHooks ::
#if MIN_VERSION_base(4,16,0)
              WDT (DsForeignsHook) =>
#endif
              Hooks

class
#if __GLASGOW_HASKELL__ >= 903
  m @ Hooks =>
#endif
  HasHooks m where
    getHooks :: m Hooks

class ContainsHooks a where
    extractHooks :: a -> Hooks
