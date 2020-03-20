{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
{-# OPTIONS -fno-enable-rewrite-rules #-}
#endif

module TcInstDcls ( tcInstDecls1 ) where

import GHC.Hs
import TcRnTypes
import TcEnv( InstInfo )
import TcDeriv

-- We need this because of the mutual recursion
-- between TcTyClsDecls and TcInstDcls
tcInstDecls1 :: [LInstDecl GhcRn]
             -> TcM (TcGblEnv, [InstInfo GhcRn], [DerivInfo])
