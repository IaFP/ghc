{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators #-}
#endif
-- | This module exists to work nicely with the QualifiedDo
-- extension.
-- @
-- import qualified Language.Haskell.TH.CodeDo as Code
-- myExample :: Monad m => Code m a -> Code m a -> Code m a
-- myExample opt1 opt2 =
--   Code.do
--    x <- someSideEffect               -- This one is of type `M Bool`
--    if x then opt1 else opt2
-- @
module Language.Haskell.TH.CodeDo((>>=), (>>)) where

import Language.Haskell.TH.Syntax
import Prelude(Monad)
#if MIN_VERSION_base(4,16,0)
import GHC.Types (type (@))
#endif
-- | Module over monad operator for 'Code'
(>>=) :: (
#if MIN_VERSION_base(4,16,0)
  m @ TExp b,
#endif
  Monad m) => m a -> (a -> Code m b) -> Code m b
(>>=) = bindCode
(>>) :: (
#if MIN_VERSION_base(4,16,0)
  m @ TExp b,
#endif
  Monad m) => m a -> Code m b -> Code m b
(>>)  = bindCode_
