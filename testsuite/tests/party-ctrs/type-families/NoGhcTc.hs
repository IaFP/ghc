{-# LANGUAGE AllowAmbiguousTypes     #-}      -- for pprIfTc, etc.
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveDataTypeable      #-}
{-# LANGUAGE EmptyDataDeriving       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableSuperClasses #-} -- for IsPass; see Note [NoGhcTc]
{-# LANGUAGE UndecidableInstances    #-} -- Wrinkle in Note [Trees That Grow]

module NoGhcTc where

--------------------------------------------------------------------------------
-- Below is Fluff needed to make the pprXOverLit function fail
--------------------------------------------------------------------------------

type WFT t = t ~ t

data Pass = Parsed | Renamed | Typechecked

data GhcPass (c :: Pass) where
  GhcPs :: GhcPass 'Parsed
  GhcRn :: GhcPass 'Renamed
  GhcTc :: GhcPass 'Typechecked

type GhcPs = GhcPass 'Parsed
type GhcRn = GhcPass 'Renamed
type GhcTc = GhcPass 'Typechecked

type family NoGhcTcPass (p :: Pass) :: Pass where
  NoGhcTcPass 'Typechecked = 'Renamed
  NoGhcTcPass other = other

g' :: GhcPass (NoGhcTcPass 'Renamed)
g' = GhcRn
