{-# LANGUAGE CPP #-}

{-# LANGUAGE TypeFamilies, TypeOperators, PolyKinds, UndecidableSuperClasses
    , QuantifiedConstraints, ExplicitNamespaces, UndecidableInstances #-}

module HigherOrderVanilla where
import GHC.Types (Type, Constraint)

class CM (m :: Type -> Type)

class (@) (f :: k' -> k) (a :: k') -- This works for unification.
-- type family  (@) (f :: k' -> k) (a :: k') :: Constraint

class f @ a => Cheat f a -- so does this
instance f @ a => Cheat f a -- and this

type Total f = forall a. Cheat f a

data HoT = HoT { hte :: forall m. CM m => HoTExis m
               , wge :: forall n a. (n @ Int, n @ a) => (n Int -> n a) -> n a }

data HoTExis m = forall n. (Total n, CM n, CM m) =>  HoTExis (forall a. (m @ a, n @ a) => n a -> m a)
                                                    (forall a. (m @ a, n @ a) => m a -> n a)

-- f :: CM m => HoT -> m a
outer :: (Total m, CM m) => HoT -> a -> m a
outer ht@HoT{hte=whyme, wge=wge} a =
  case whyme of
    HoTExis e l -> e $ wge (undefined l ht)


-- helper :: forall m n a. (CM m, CM n)
--        => (forall b. m b -> n b) -> HoT -> n Int -> n a
-- helper l h init = undefined

