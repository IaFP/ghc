{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module DataKinds where

import GHC.Types


data Pass = PS | RN | TC

data GhcPass (p :: Pass) where
  GhcPs :: GhcPass 'PS
  GhcRn :: GhcPass 'RN
  GhcTc :: GhcPass 'TC

type GhcPs = GhcPass 'PS
type GhcRn = GhcPass 'RN
type GhcTc = GhcPass 'TC


type family XOverLit p
type instance XOverLit GhcPs = ()
type instance XOverLit GhcRn = ()
type instance XOverLit GhcTc = ()

data HsOverLit p = OverLit (XOverLit p)

class C p => C (HsOverLit p) where
  
