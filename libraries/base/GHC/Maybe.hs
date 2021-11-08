{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeConstructors, DatatypeContexts, ExistentialQuantification, StandaloneDeriving, RankNTypes #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Maybe type
module GHC.Maybe
   ( Maybe (..)
   )
where

import GHC.Integer () -- See Note [Depend on GHC.Integer] in GHC.Base
import GHC.Classes
import GHC.Types

default ()

-------------------------------------------------------------------------------
-- Maybe type
-------------------------------------------------------------------------------

-- | The 'Maybe' type encapsulates an optional value.  A value of type
-- @'Maybe' a@ either contains a value of type @a@ (represented as @'Just' a@),
-- or it is empty (represented as 'Nothing').  Using 'Maybe' is a good way to
-- deal with errors or exceptional cases without resorting to drastic
-- measures such as 'Prelude.error'.
--
-- The 'Maybe' type is also a monad.  It is a simple kind of error
-- monad, where all errors are represented by 'Nothing'.  A richer
-- error monad can be built using the 'Data.Either.Either' type.
--
data Maybe a  =  Nothing | Just a
  deriving ( Eq  -- ^ @since 2.01
           , Ord -- ^ @since 2.01
           )

-- deriving instance Ord a => Ord (Maybe a)
-- deriving instance Eq a => Eq (Maybe a)
{-
data Ord a => MaybeO a  =  NothingO | JustO a

instance Eq (MaybeO a) where
  NothingO == NothingO = True
  NothingO == _        = False
  JustO a  == JustO b  = a == b
  JustO _  == _        = False

deriving instance Ord (MaybeO a)

data Cld = MkCld (MaybeO Bool)
         | MkCld' (Maybe Int)

data Cld2 a = Cld2 (Maybe a)
data Cld3 a = Cld3 (MaybeO a)

data Blah p a = MkBlah {unBlah :: p a}


data MaybeO' a  =  NothingO' | Ord a => JustO' a

instance Ord a => Eq (MaybeO' a) where
  NothingO' == NothingO' = True
  NothingO' == _        = False
  JustO' a  == JustO' b  = a == b
  JustO' _  == _        = False

deriving instance Ord a => Ord (MaybeO' a)


instance Ord a => Ord (MaybeO a) where
  NothingO <= NothingO = True
  NothingO <= JustO _ = False
  JustO a <= JustO b = a <= b
  JustO _ <= NothingO = False


This doesn't work. :(
data Ord a => MaybeO' a  =  NothingO' | Ord a => JustO' a


instance Ord a => Ord (MaybeO' a) where
  NothingO' <= NothingO' = True
  NothingO' <= JustO' _ = False
  JustO' a <= JustO' b = a <= b
  JustO' _ <= NothingO' = False
-}

-- newtype MN a = MkMN {unMN :: MaybeO a}

-- instance (Ord a) => Eq (MN a) where
--   MkMN (NothingO) == MkMN (NothingO) = True
--   MkMN (NothingO) == _ = False
--   MkMN (JustO a)  == MkMN (JustO b) = a == b
--   MkMN (JustO a)  == _ = False

-- deriving instance (Ord a) => Eq (MayBeN a)


-- newtype NT f a = MkNT {unNT :: f a}
-- newtype NT1 f a = MkNT1 {unNT1 :: Ord a => f a}
