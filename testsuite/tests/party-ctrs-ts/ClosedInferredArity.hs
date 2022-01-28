{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module ClosedInferredArity where

data Fix f = In (f (Fix f))

-- Closed TFs have type var kind inference
type family F a where
  F a = Fix a

