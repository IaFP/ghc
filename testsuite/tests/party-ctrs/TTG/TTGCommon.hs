{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators, TypeFamilies, TypeFamilyDependencies #-}
#endif

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE DataKinds #-} 
{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

module TTGCommon where

import GHC.Types

class Printable p where
  print :: p -> String

data Pass = Ps | Rn | Tc

data GenLocated l a = L l a

type GhcPs = GhcPass 'Ps
type GhcRn = GhcPass 'Rn
type GhcTc = GhcPass 'Tc

data GhcPass (p :: Pass) where
  GhcPs :: GhcPass 'Ps
  GhcRn :: GhcPass 'Rn
  GhcTc :: GhcPass 'Tc

type family XRec p a = r | r -> a
type family Anno a = b

type instance XRec (GhcPass p) a = GenLocated (Anno a) a
