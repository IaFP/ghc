{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators, DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]


module Type where

{-------------------------------------------------------------------------------
Debugging the error:

    compiler/GHC/Hs/Type.hs:227:78: error:
        • Couldn't match kind ‘Constraint’ with ‘Pass’
          When matching types
            p2 :: Pass
            () :: Constraint :: Constraint
          Expected: [LHsTyVarBndr flag (GhcPass p2)]
            Actual: [LHsTyVarBndr flag (NoGhcTc GhcRn)]
        • In the first argument of ‘hsLTyVarNames’, namely ‘bndrs’
          In the expression: hsLTyVarNames bndrs
          In an equation for ‘hsOuterTyVarNames’:
              hsOuterTyVarNames (HsOuterExplicit {hso_bndrs = bndrs})
                = hsLTyVarNames bndrs
        |
    227 | hsOuterTyVarNames (HsOuterExplicit{hso_bndrs = bndrs})       = hsLTyVarNames bndrs
        |                                                                              ^^^^^
    
when running:
    hb stage1:lib:ghc

found in:
    GHC/Hs/Type.hs

on branch:
    closed-tfs

Repro:

_build/stage0/bin/ghc -Wall -Wcompat -hisuf p_hi -osuf p_o -hcsuf p_hc -static -prof -eventlog -hide-all-packages -no-user-package-db '-package-env -' '-package-db _build/stage1/lib/package.conf.d' '-this-unit-id ghc-9.3' '-package-id array-0.5.4.0' '-package-id base-4.16.0.0' '-package-id binary-0.8.8.0' '-package-id bytestring-0.11.1.0' '-package-id containers-0.6.4.1' '-package-id deepseq-1.4.4.0' '-package-id directory-1.3.6.1' '-package-id exceptions-0.10.4' '-package-id filepath-1.4.2.1' '-package-id ghc-boot-9.3' '-package-id ghc-heap-9.3' '-package-id ghc-prim-0.8.0' '-package-id ghci-9.3' '-package-id hpc-0.6.1.0' '-package-id parsec-3.1.14.0' '-package-id process-1.6.13.1' '-package-id stm-2.5.0.1' '-package-id template-haskell-2.18.0.0' '-package-id terminfo-0.4.1.5' '-package-id time-1.11.1.1' '-package-id transformers-0.5.6.2' '-package-id unix-2.7.2.2' -i -i/home/alex/PTC/ghc/_build/stage1/compiler/build -i/home/alex/PTC/ghc/_build/stage1/compiler/build/autogen -i/home/alex/PTC/ghc/compiler -Irts/include -I_build/stage1/compiler/build -I_build/stage1/compiler/build/. -Icompiler/. -I/home/alex/PTC/ghc/_build/stage1/lib/x86_64-linux-ghc-9.3.20220328/process-1.6.13.1/include -I/home/alex/PTC/ghc/_build/stage1/lib/x86_64-linux-ghc-9.3.20220328/unix-2.7.2.2/include -I/home/alex/PTC/ghc/_build/stage1/lib/x86_64-linux-ghc-9.3.20220328/time-1.11.1.1/include -I/home/alex/PTC/ghc/_build/stage1/lib/x86_64-linux-ghc-9.3.20220328/bytestring-0.11.1.0/include -I/home/alex/PTC/ghc/_build/stage1/lib/x86_64-linux-ghc-9.3.20220328/base-4.16.0.0/include -I/home/alex/PTC/ghc/_build/stage1/lib/x86_64-linux-ghc-9.3.20220328/ghc-bignum-1.3/include -I/home/alex/PTC/ghc/_build/stage1/lib/x86_64-linux-ghc-9.3.20220328/rts-1.0.2/include -optP-include -optP_build/stage1/compiler/build/autogen/cabal_macros.h -optP-DHAVE_INTERNAL_INTERPRETER -optP-DCAN_LOAD_DLL -outputdir _build/stage1/compiler/build -fdiagnostics-color=always -Wnoncanonical-monad-instances -optc-Wno-error=inline -optP-Wno-nonportable-include-path -c compiler/GHC/Hs/Type.hs -o _build/stage1/compiler/build/GHC/Hs/Type.p_o -O2 -H32m -Wall -Wno-name-shadowing -Wnoncanonical-monad-instances -Wnoncanonical-monoid-instances -this-unit-id ghc -XHaskell2010 -XNoImplicitPrelude -XBangPatterns -XScopedTypeVariables -XMonoLocalBinds -no-global-package-db -package-db=/home/alex/PTC/ghc/_build/stage1/lib/package.conf.d -ghcversion-file=rts/include/ghcversion.h -Wno-deprecated-flags -Wcpp-undef -XPartialTypeConstructors -XConstrainedClassMethods -XUndecidableInstances -XExplicitNamespaces -XTypeOperators -fforce-recomp -ddump-tc-trace > Type.out

-------------------------------------------------------------------------------}

data Name    = Name
type RdrName = Name
type Id      = Name

type WFT t = t ~ t

data Pass = Parsed | Renamed | Typechecked

data GhcPass (c :: Pass) where
  GhcPs :: GhcPass 'Parsed
  GhcRn :: GhcPass 'Renamed
  GhcTc :: GhcPass 'Typechecked

type GhcRn = GhcPass 'Renamed

-- | Maps the "normal" id type for a given GHC pass
type family IdGhcP pass where
  IdGhcP 'Parsed      = RdrName
  IdGhcP 'Renamed     = Name
  IdGhcP 'Typechecked = Id

type family IdP p
type instance IdP (GhcPass p) = IdGhcP p

type family NoGhcTc (p :: *)

type instance NoGhcTc (GhcPass pass) = GhcPass (NoGhcTcPass pass)

type family NoGhcTcPass (p :: Pass) :: Pass where
  NoGhcTcPass 'Typechecked = 'Renamed
  NoGhcTcPass other        = other

data HsTyVarBndr flag pass = HsTyVarBndr

type family XHsOuterExplicit    x flag

data NoExtField = NoExtField
data EpAnnForallTy = EpAnnForallTy
data TyVar = TyVar
data VarBndr v f = VarBndr

type instance XHsOuterExplicit (GhcPass 'Parsed) _    = EpAnnForallTy
type instance XHsOuterExplicit GhcRn _    = NoExtField
type instance XHsOuterExplicit (GhcPass 'Typechecked) flag = [VarBndr TyVar flag]

data HsOuterTyVarBndrs flag pass
  =
  (WFT (XHsOuterExplicit pass flag), WFT (HsTyVarBndr flag (NoGhcTc pass))) => 
    HsOuterExplicit
    { hso_bndrs     :: [HsTyVarBndr flag (NoGhcTc pass)] }

hsLTyVarName :: HsTyVarBndr flag (GhcPass p) -> IdP (GhcPass p)
hsLTyVarName = undefined

hsLTyVarNames :: [HsTyVarBndr flag (GhcPass p)] -> [IdP (GhcPass p)]
hsLTyVarNames = map hsLTyVarName

hsOuterTyVarNames :: HsOuterTyVarBndrs flag GhcRn -> [Name]
hsOuterTyVarNames (HsOuterExplicit{hso_bndrs = bndrs}) = hsLTyVarNames bndrs

-- bndrs :: HsTyVarBndr flag (NoGhcTc GhcRn)
-- ~ 



{--------------------------------------------------------------------------------

    compiler/GHC/Hs/Type.hs:240:64: error:
        • Couldn't match type ‘'Parsed’ with ‘() :: Constraint’
          Expected: [LHsTyVarBndr flag (NoGhcTc GhcPs)]
            Actual: [LHsTyVarBndr flag GhcPs]
        • In the ‘hso_bndrs’ field of a record
          In the expression:
            HsOuterExplicit {hso_xexplicit = an, hso_bndrs = bndrs}
          In an equation for ‘mkHsOuterExplicit’:
              mkHsOuterExplicit an bndrs
                = HsOuterExplicit {hso_xexplicit = an, hso_bndrs = bndrs}
        |
    240 |                                              , hso_bndrs     = bndrs }
        |                                                                ^^^^^
    
    compiler/GHC/Hs/Type.hs:289:37: error:
        • Couldn't match type ‘() :: Constraint’ with ‘Name’
          Expected: GenLocated (Anno (() :: Constraint)) Name
            Actual: LIdP GhcRn
        • In the first argument of ‘unLoc’, namely ‘v’
          In the expression: unLoc v
          In an equation for ‘getName’: getName (UserTyVar _ _ v) = unLoc v
        |
    289 |   getName (UserTyVar _ _ v) = unLoc v
        |                                     ^
    
    compiler/GHC/Hs/Type.hs:290:41: error:
        • Couldn't match type ‘() :: Constraint’ with ‘Name’
          Expected: GenLocated (Anno (() :: Constraint)) Name
            Actual: LIdP GhcRn
        • In the first argument of ‘unLoc’, namely ‘v’
          In the expression: unLoc v
          In an equation for ‘getName’:
              getName (KindedTyVar _ _ v _) = unLoc v
        |
    290 |   getName (KindedTyVar _ _ v _) = unLoc v
        |                                         ^
    
    compiler/GHC/Hs/Type.hs:335:45: error:
        • Couldn't match type ‘Name’ with ‘() :: Constraint’
          Expected: LIdP GhcRn
            Actual: LocatedAn an0 Name
        • In the third argument of ‘HsTyVar’, namely
            ‘(noLocA oneDataConName)’
          In the expression:
            HsTyVar noAnn NotPromoted (noLocA oneDataConName)
          In an equation for ‘oneDataConHsTy’:
              oneDataConHsTy = HsTyVar noAnn NotPromoted (noLocA oneDataConName)
        |
    335 | oneDataConHsTy = HsTyVar noAnn NotPromoted (noLocA oneDataConName)
        |                                             ^^^^^^^^^^^^^^^^^^^^^
    
    compiler/GHC/Hs/Type.hs:338:46: error:
        • Couldn't match type ‘Name’ with ‘() :: Constraint’
          Expected: LIdP GhcRn
            Actual: LocatedAn an0 Name
        • In the third argument of ‘HsTyVar’, namely
            ‘(noLocA manyDataConName)’
          In the expression:
            HsTyVar noAnn NotPromoted (noLocA manyDataConName)
          In an equation for ‘manyDataConHsTy’:
              manyDataConHsTy
                = HsTyVar noAnn NotPromoted (noLocA manyDataConName)
        |
    338 | manyDataConHsTy = HsTyVar noAnn NotPromoted (noLocA manyDataConName)
        |                                              ^^^^^^^^^^^^^^^^^^^^^^
    
    compiler/GHC/Hs/Type.hs:347:52: error:
        • Couldn't match type ‘Name’ with ‘() :: Constraint’
          Expected: LIdP GhcRn
            Actual: GenLocated (Anno (() :: Constraint)) Name
        • In the pattern: L _ n
          In the pattern: HsTyVar _ _ (L _ n)
          In the pattern: L _ (HsTyVar _ _ (L _ n))
        |
    347 | isUnrestricted (arrowToHsType -> L _ (HsTyVar _ _ (L _ n))) = n == manyDataConName
        |                                                    ^^^^^
    
    compiler/GHC/Hs/Type.hs:401:28: error:
        • Couldn't match kind ‘Constraint’ with ‘Pass’
          When matching types
            p0 :: Pass
            () :: Constraint :: Constraint
          Expected: [LHsTyVarBndr Specificity (GhcPass p0)]
            Actual: [LHsTyVarBndr Specificity (NoGhcTc (GhcPass 'Renamed))]
        • In the first argument of ‘hsLTyVarNames’, namely
            ‘(hsOuterExplicitBndrs outer_bndrs)’
          In the second argument of ‘(++)’, namely
            ‘hsLTyVarNames (hsOuterExplicitBndrs outer_bndrs)’
          In the expression:
            nwcs ++ hsLTyVarNames (hsOuterExplicitBndrs outer_bndrs)
        |
    401 |   = nwcs ++ hsLTyVarNames (hsOuterExplicitBndrs outer_bndrs)
        |                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    
    compiler/GHC/Hs/Type.hs:407:20: error:
        • Couldn't match kind ‘Constraint’ with ‘Pass’
          When matching types
            p1 :: Pass
            () :: Constraint :: Constraint
          Expected: [LHsTyVarBndr Specificity (GhcPass p1)]
            Actual: [LHsTyVarBndr Specificity (NoGhcTc (GhcPass 'Renamed))]
        • In the first argument of ‘hsLTyVarNames’, namely
            ‘(hsOuterExplicitBndrs outer_bndrs)’
          In the expression: hsLTyVarNames (hsOuterExplicitBndrs outer_bndrs)
          In an equation for ‘hsScopedTvs’:
              hsScopedTvs (L _ (HsSig {sig_bndrs = outer_bndrs}))
                = hsLTyVarNames (hsOuterExplicitBndrs outer_bndrs)
        |
    407 |   = hsLTyVarNames (hsOuterExplicitBndrs outer_bndrs)
        |                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    
    compiler/GHC/Hs/Type.hs:429:12: error:
        • Couldn't match type ‘() :: Constraint’ with ‘Name’
          Expected: [Name]
            Actual: [IdP (GhcPass 'Renamed)]
        • In the second argument of ‘(++)’, namely ‘hsLTyVarNames tvs’
          In the expression: kvs ++ hsLTyVarNames tvs
          In an equation for ‘hsAllLTyVarNames’:
              hsAllLTyVarNames (HsQTvs {hsq_ext = kvs, hsq_explicit = tvs})
                = kvs ++ hsLTyVarNames tvs
        |
    429 |   = kvs ++ hsLTyVarNames tvs
        |            ^^^^^^^^^^^^^^^^^
    
    compiler/GHC/Hs/Type.hs:1032:42: error:
        • Couldn't match kind ‘Constraint’ with ‘Pass’
          When matching types
            p3 :: Pass
            () :: Constraint :: Constraint
          Expected: [LHsTyVarBndr Specificity (GhcPass p3)]
            Actual: [LHsTyVarBndr Specificity (NoGhcTc (GhcPass p))]
        • In the second argument of ‘mkHsForAllInvisTele’, namely ‘bndrs’
          In the first argument of ‘pprHsForAll’, namely
            ‘(mkHsForAllInvisTele noAnn bndrs)’
          In the expression:
            pprHsForAll (mkHsForAllInvisTele noAnn bndrs) Nothing
         |
    1032 |   pprHsForAll (mkHsForAllInvisTele noAnn bndrs) Nothing
         |    


--------------------------------------------------------------------------------}
