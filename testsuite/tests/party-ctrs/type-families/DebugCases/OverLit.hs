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


module Overlit where

{-------------------------------------------------------------------------------
Debugging the error:
    compiler/GHC/Hs/Lit.hs:109:55: error:
        • Could not deduce (OutputableBndr (IdGhcP (() :: Constraint)))
            arising from a use of ‘pprExpr’
          from the context: ($wf'XOverLit (GhcPass p), GhcPass @ p)
            bound by the type signature for:
                       pprXOverLit :: forall (p :: Pass).
                                      ($wf'XOverLit (GhcPass p), $wf'XOverLit (GhcPass p), GhcPass @ p,
                                       WFT (XOverLit (GhcPass p))) =>
                                      GhcPass p -> XOverLit (GhcPass p) -> SDoc
            at compiler/GHC/Hs/Lit.hs:(102,1)-(106,43)
          or from: p ~ 'Typechecked
            bound by a pattern with constructor: GhcTc :: GhcPass 'Typechecked,
                     in an equation for ‘pprXOverLit’
            at compiler/GHC/Hs/Lit.hs:109:13-17
        • In the expression: pprExpr witness
          In an equation for ‘pprXOverLit’:
              pprXOverLit GhcTc OverLitTc {ol_witness = witness}
                = pprExpr witness
        |
    109 | pprXOverLit GhcTc OverLitTc{ ol_witness = witness } = pprExpr witness
        |

when running
    hb stage1:lib:ghc

found in
    GHC/Hs/Lit.hs

on branch
    closed-tfs
-------------------------------------------------------------------------------}

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

type family XOverLit p
type instance XOverLit GhcPs = ()
type instance XOverLit GhcRn = ()
type instance XOverLit GhcTc = OverLitTc

data SDoc = SDoc

class Outputable a where
    ppr :: a -> SDoc

class Outputable a => OutputableBndr a where
   pprBndr :: () -> a -> SDoc
   pprBndr _b x = ppr x

   pprPrefixOcc, pprInfixOcc :: a -> SDoc
      -- Print an occurrence of the name, suitable either in the
      -- prefix position of an application, thus   (f a b) or  ((+) x)
      -- or infix position,                 thus   (a `f` b) or  (x + y)

   bndrIsJoin_maybe :: a -> Maybe Int
   bndrIsJoin_maybe _ = Nothing

data RdrName = RdrName
data Name    = Name
data Id      = Id

type family IdGhcP pass where
  IdGhcP 'Parsed      = RdrName
  IdGhcP 'Renamed     = Name
  IdGhcP 'Typechecked = Id

type OutputableBndrId p = OutputableBndr (GhcPass p)

pprExpr :: (
  WFT (XOverLit (GhcPass p)),
  OutputableBndrId p) => HsExpr (GhcPass p) -> SDoc
pprExpr e = undefined

data HsExpr p = HsExpr

data OverLitTc
  = OverLitTc { ol_witness    :: HsExpr GhcTc }

{--------------------------------------------------------------------------------
Actually failing function is pprXOverlit
--------------------------------------------------------------------------------

FIX:
  This is very odd. The use of `pprExpr` expects
  the predicate `OutputableBndrId p` to be satisfiable.
  For whatever reason, this predicate *was* being satisfied
  prior to my Closed TFs work,  but now needs to be added
  as a constraint to this function. My guess is that
  when you are in the GhcTc branch, we know `p ~ 'Typechecked`,
  and there must be an instance of (all the constraints constituting)
  `OutputableBndrId 'Typechecked`? I do not see how my Closed TFs work
  would interfere with this.
-------------------------------------------------------------------------------}

pprXOverLit :: OutputableBndrId p => GhcPass p -> XOverLit (GhcPass p) -> SDoc
pprXOverLit GhcTc OverLitTc{ ol_witness = witness } = pprExpr witness
