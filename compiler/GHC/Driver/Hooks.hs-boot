{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE ExplicitNamespaces, TypeOperators, UndecidableSuperClasses #-}
#endif
module GHC.Driver.Hooks where

import GHC.Prelude ()
#if MIN_VERSION_base(4,16,0)
import GHC.Types (type(@))
#endif

data Hooks

emptyHooks :: Hooks

class
#if __GLASGOW_HASKELL__ >= 903
  m @ Hooks =>
#endif
  HasHooks m where
    getHooks :: m Hooks

class ContainsHooks a where
    extractHooks :: a -> Hooks
