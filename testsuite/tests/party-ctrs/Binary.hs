{-# LANGUAGE CPP, FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds, RankNTypes, DeriveGeneric #-}

{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies, ConstrainedClassMethods, UndecidableSuperClasses #-}

module Binary where
import Data.Foldable (mapM_)
import GHC.Generics
import GHC.Types (type (@@), Total)

data Decoder a = Fail String String
              | Partial (Maybe String -> Decoder a)
              | Done String a
              | BytesRead Int (Int -> Decoder a)

-- unrolled codensity/state monad
newtype Get a = C { runCont :: forall r.
                               String ->
                               Success a r ->
                               Decoder r }
instance Functor Get where
  fmap = fmapG

fmapG :: (a -> b) -> Get a -> Get b
fmapG f m = C $ \i ks -> runCont m i (\i' a -> ks i' (f a))

type Success a r = String -> a -> Decoder r

data PairS a = PairS a
newtype PutM a = Put { unPut :: PairS a }

-- | Put merely lifts Builder into a Writer monad, applied to ().
type Put = PutM ()


class GBinaryPut f where
    gput :: f t -> Put

class GBinaryGet f where
    gget :: Get (f t)

class Binary t where
    put :: t -> Put
    get :: Get t

    default put :: (Total (Rep t), Generic t, GBinaryPut (Rep t)) => t -> Put
    put = gput . from

    default get :: (Total (Rep t), Generic t, GBinaryGet (Rep t)) => Get t
    get = to `fmap` gget


data Blah = Bleh | Meh | Deh
  deriving (Eq, Show, Generic)

instance Binary Blah where
