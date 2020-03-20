{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
#endif

module ClosureEnv where

import Name
import NameEnv
import GHCi.RemoteTypes

{-
  Linking interpretables into something we can run
-}

type ClosureEnv = NameEnv (Name, ForeignHValue)

emptyClosureEnv :: ClosureEnv
emptyClosureEnv = emptyNameEnv

extendClosureEnv :: ClosureEnv -> [(Name,ForeignHValue)] -> ClosureEnv
extendClosureEnv cl_env pairs
  = extendNameEnvList cl_env [ (n, (n,v)) | (n,v) <- pairs]
