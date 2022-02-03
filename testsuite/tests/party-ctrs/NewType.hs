{-# LANGUAGE PolyKinds, StandaloneDeriving, GeneralizedNewtypeDeriving
           , RoleAnnotations, RankNTypes, ExistentialQuantification #-}

module NewType where
import GHC.Types (Total)

newtype Const a b = Const { getConst :: a }
  deriving (Eq, Ord)


newtype NT f a = MkNT {unNT :: f a}
newtype NT' a f = MkNT' {unNT' :: f a}

newtype Compose f g a = MkCompose {unCompose :: f (g a)}
newtype Kleisli m a b = MkKleisli {runKleisli :: a -> m b}


newtype T1 a b = MkT1 {unT1 :: T2 a b }
newtype T2 a b = MkT2 {unT2 :: T1 a b }


type Errors e = Lift (Const e)

data Lift f a = Pure a | Other (f a)

failure :: e -> Errors e a
failure e = Other (Const e)



-- type role NT representational nominal
newtype Ord a => NTO a = MkNTO { unNTO :: a }
-- This fails
-- How are newtypes represented at core level?

-- MkNT :: f @@ a => f a -> MKNT f a
-- unNT :: f @@ a => MKNT f a -> f @@ a => f a
-- MkNT f a ~ f a

-- instance (Total f, Eq (f a)) => Eq (NT f a) where
--   MkNT f == MkNT f' = f == f'
