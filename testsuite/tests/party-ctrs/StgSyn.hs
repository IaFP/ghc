{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeConstructors #-}

-- Some examples from ghc Stg
module StgSyn where


data GenStgRhs a = StgRhsClosure (GenStgExpr )
                 |
