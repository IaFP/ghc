{-# LANGUAGE KindSignatures, PolyKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE RankNTypes, FlexibleContexts , ConstrainedClassMethods #-}
{-# LANGUAGE PartialTypeConstructors, QuantifiedConstraints, DatatypeContexts
           , DefaultSignatures, ExistentialQuantification, ConstraintKinds, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass, MagicHash, UnboxedTuples, DataKinds, TypeApplications, UnliftedNewtypes, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module PTraverse where


-- import GHC.Types (Constraint)
import GHC.Types (type (@@), Total)
import qualified Data.List as List
import Data.List.NonEmpty
import GHC.Base (Applicative (..))

class (Functor t, Foldable t, Applicative f) => PTraversable t f where
    {-# MINIMAL ptraverse | psequenceA #-}
    ptraverse :: (t @@ f b) => (a -> f b) -> t a -> f (t b)
    {-# INLINE ptraverse #-}
    ptraverse f = psequenceA . fmap f

    psequenceA :: t (f a) -> f (t a)
    {-# INLINE psequenceA #-}
    psequenceA = ptraverse id

class (Functor t, Foldable t, Monad m, PTraversable t m) => PTraversableM t m where
    pmapM :: (t @@ m b) => (a -> m b) -> t a -> m (t b)
    {-# INLINE pmapM #-}
    pmapM = ptraverse

    psequence :: t (m a) -> m (t a)
    {-# INLINE psequence #-}
    psequence = psequenceA

instance (Applicative f) => PTraversable Maybe f where
    ptraverse _ Nothing = pure Nothing
    ptraverse f (Just x) = Just <$> f x

instance (Monad f) => PTraversableM Maybe f


instance (Applicative f) => PTraversable [] f where
    {-# INLINE ptraverse #-} -- so that traverse can fuse
    ptraverse f = List.foldr cons_f (pure [])
      where cons_f x ys = liftA2 (:) (f x) ys

instance (Monad f) => PTraversableM [] f


instance (Applicative f) => PTraversable NonEmpty f where
  -- ptraverse :: (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  ptraverse f ~(a :| as) = liftA2 (:|) (f a) (ptraverse f as)

instance (Monad f, Total f) => PTraversableM NonEmpty f

-- nelist :: NonEmpty' String
-- nelist = "a" :|| ["b", "c", "d"]

-- main :: IO ()
-- main =
--   -- psequence $ ptraverse putStrLn nelist
--   putStrLn "Thats all folks!"
