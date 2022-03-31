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

when running
    hb stage1:lib:ghc

found in
    GHC/Hs/Type.hs

on branch
    closed-tfs
-------------------------------------------------------------------------------}

