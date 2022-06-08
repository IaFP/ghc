{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes, FlexibleInstances
             , TypeSynonymInstances
             , FlexibleContexts, ExistentialQuantification
             , ScopedTypeVariables, GeneralizedNewtypeDeriving
             , StandaloneDeriving
             , MultiParamTypeClasses
             , UndecidableInstances
             , ScopedTypeVariables, CPP, DeriveDataTypeable
             , PatternGuards
  #-}
{-# LANGUAGE  QuantifiedConstraints, DatatypeContexts
           , DefaultSignatures, ConstraintKinds #-}
{-# LANGUAGE LambdaCase, DeriveAnyClass, DataKinds, TypeApplications, UnliftedNewtypes, TypeFamilies, TypeOperators, PolyKinds #-}


module Existentials2 where
import GHC.Types (WDT)

type family F a
class D b

class {-WDT (F b) =>-} C b where
  fff :: (D (F b) => Maybe b) -> Maybe b

-- should fff be typed: forall b. (C b, WD'F b) => (D (F b) => Maybe b) -> Maybe b
-- or
-- should fff be typed: forall b. (C b) => ((D (F b), WD'F b) => Maybe b) -> Maybe b

newtype NT = MkNT {unNT :: forall b. C b => Maybe b}

nt_term :: NT
nt_term = MkNT $ fff Nothing
