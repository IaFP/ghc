{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances, IncoherentInstances #-}

module RecordField where

import GHC.Types (type (@), Total)
import Data.Data

type SrcSpan = (Int, Int)

data GenLocated l e = L l e
  deriving (Eq, Ord, Data)

deriving instance Functor (GenLocated p)
instance Foldable (GenLocated p) where
  foldMap f = foldr (mappend . f) mempty

instance Traversable (GenLocated p) where
  traverse f = sequenceA . fmap f

type Located = GenLocated SrcSpan

type HsRecField p arg = HsRecField' p arg

type LHsRecField  p arg = Located (HsRecField  p arg)

data HsRecFields p arg         -- A bunch of record fields
                                --      { x = 3, y = True }
        -- Used for both expressions and patterns
  = HsRecFields { rec_flds   :: ([p], [arg]), -- [LHsRecField p arg],
                  rec_dotdot :: Maybe (Located Int) }  -- Note [DotDot fields]
  deriving (Functor, Data)

instance (Foldable (HsRecFields p)) where
  foldMap f = foldr (mappend . f) mempty  
instance (Traversable (HsRecFields p)) where
  traverse f = sequenceA . fmap f

-- instance Total (HsRecFields p)

data HsRecField' id arg = HsRecField {
        hsRecFieldLbl :: Located id,
        hsRecFieldArg :: arg,           -- ^ Filled in by renamer when punning
        hsRecPun      :: Bool           -- ^ Note [Punning]
  }
  deriving (Data, Functor)
instance Foldable (HsRecField' id) where
    foldMap f = foldr (mappend . f) mempty

instance Traversable (HsRecField' id) where
  traverse f = sequenceA . fmap f

-- instance Total (HsRecField' id)

