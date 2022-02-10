{-# LANGUAGE DatatypeContexts, TypeFamilies, PolyKinds, TypeFamilies, TypeOperators #-}

module Con2Tag where
import GHC.Types (Constraint)

type family (@) (t :: k' -> k) (u:: k') :: Constraint
-- type instance MaybeOrd @@ a = ()

data (MaybeOrd @ a, Ord a) => MaybeOrd a  =  NothingO | JustO a
  deriving ( Eq
           , Ord
           )
