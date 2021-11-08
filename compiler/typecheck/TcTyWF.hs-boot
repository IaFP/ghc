-- Module imports form a cycle:
--          module ‘BuildTyCl’ (compiler/iface/BuildTyCl.hs)
--         imports ‘TcTyWF’ (compiler/typecheck/TcTyWF.hs)
--   which imports ‘TcSMonad’ (compiler/typecheck/TcSMonad.hs)
--   which imports ‘Inst’ (compiler/typecheck/Inst.hs)
--   which imports ‘TcHsSyn’ (compiler/typecheck/TcHsSyn.hs)
--   which imports ‘BuildTyCl’ (compiler/iface/BuildTyCl.hs)
{-# LANGUAGE CPP, ExplicitNamespaces, TypeOperators #-}
module TcTyWF where

#if __GLASGOW_HASKELL__ >= 810
import GHC.Types (type (@@))
#endif

import TcType
import GHC.Base (Monad)

genAtAtConstraints :: (Monad m
#if __GLASGOW_HASKELL__ >= 810
                      , m @@ ThetaType, m @@ [(Type, ThetaType)]
#endif
                      ) => Type ->  m (Type, ThetaType)
