{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash,RoleAnnotations, UndecidableInstances, QuantifiedConstraints #-}

module DataFamilies where
import GHC.Types

data family Sing (a :: k)

data instance Sing (a::Bool) where
  ST :: Sing 'True
  SF :: Sing 'False

-- type instance (Sing @Bool) @ (a :: Bool) = ()::Constraint

data family URec (a :: Type) (p :: k)
data instance URec Char (p :: k) = UChar { uChar :: Char }


data family DF a b
data instance Ord a => DF a [b] = L a | B a (DF a [b]) (DF a [b])

deriving instance Functor (DF a)
