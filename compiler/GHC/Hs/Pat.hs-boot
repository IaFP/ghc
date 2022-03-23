{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators, TypeFamilies, PartialTypeConstructors #-}
#endif

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

module GHC.Hs.Pat where

import GHC.Utils.Outputable
import GHC.Hs.Extension (  OutputableBndrId, GhcPass )

import Language.Haskell.Syntax.Pat
#if MIN_VERSION_base(4,16,0)
import GHC.Types (WFT)
import Language.Haskell.Syntax.Extension (XOverLit)
#endif

instance (
#if MIN_VERSION_base(4,16,0)
  WFT (XOverLit (GhcPass p)),
#endif
  OutputableBndrId p) => Outputable (Pat (GhcPass p))

pprLPat :: (
#if MIN_VERSION_base(4,16,0)
  WFT (XOverLit (GhcPass p)),
#endif
  OutputableBndrId p) => LPat (GhcPass p) -> SDoc

