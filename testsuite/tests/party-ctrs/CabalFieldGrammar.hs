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
{-# LANGUAGE PartialTypeConstructors, QuantifiedConstraints, DatatypeContexts
           , DefaultSignatures, ConstraintKinds, FunctionalDependencies, DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass, DataKinds, TypeApplications, UnliftedNewtypes, TypeFamilies, TypeOperators, PolyKinds #-}

module CabalFieldGrammar where

import Data.Functor.Identity (Identity (..))
import Data.Monoid (Sum (..), Product (..), Endo (..))
import Data.Typeable
import Data.Data
import GHC.Generics
import Data.Coerce (coerce, Coercible)

import CompactLens
import GHC.Base
import qualified Control.Monad.Fail as Fail
import GHC.Types (type (@@), Total)


class (MonadPlus m, Fail.MonadFail m) => CabalParsing m

class Parsec a where
    parsec :: (CabalParsing m, Total m) => m a

instance Parsec a => Parsec (Identity a) where
  parsec  = Identity <$> parsec

class Pretty a where
  pretty :: a -> String

instance Pretty a => Pretty (Identity a) where
  pretty (Identity a) = pretty a

class Newtype o n | n -> o where
    pack   :: o -> n
    default pack :: Coercible o n => o -> n
    pack = coerce

    unpack :: n -> o
    default unpack :: Coercible n o => n -> o
    unpack = coerce

instance Newtype a (Identity a)
instance Newtype a (Sum a)
instance Newtype a (Product a)
instance Newtype (a -> a) (Endo a)

ala :: (Newtype o n, Newtype o' n') => (o -> n) -> ((o -> n) -> b -> n') -> (b -> o')
ala pa hof = alaf pa hof id

alaf :: (Newtype o n, Newtype o' n') => (o -> n) -> ((a -> n) -> b -> n') -> (a -> o) -> (b -> o')
alaf _ hof f = unpack . hof (pack . f)

pack' :: Newtype o n => (o -> n) -> o -> n
pack' _ = pack

unpack' :: Newtype o n => (o -> n) -> n -> o
unpack' _ = unpack

type FieldName = String
type ShortText = String
toShortText = id
fromShortText = id

data CabalSpecVersion
    = CabalSpecV1_0 -- ^ this is older than 'CabalSpecV1_2'
    | CabalSpecV1_2 -- ^ new syntax (sections)
    | CabalSpecV1_4
    | CabalSpecV1_6
    | CabalSpecV1_8
    | CabalSpecV1_10
    | CabalSpecV1_12
    -- 1.16 -- 1.14: no changes
    | CabalSpecV1_18
    | CabalSpecV1_20
    | CabalSpecV1_22
    | CabalSpecV1_24
    | CabalSpecV2_0
    | CabalSpecV2_2
    | CabalSpecV2_4
    | CabalSpecV3_0
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data, Generic)

class FieldGrammar g where
    blurFieldGrammar :: ALens' a b -> g b c -> g a c

    -- | Field which should be defined, exactly once.
    uniqueFieldAla
        :: (Parsec b, Pretty b, Newtype a b)
        => FieldName   -- ^ field name
        -> (a -> b)    -- ^ 'Newtype' pack
        -> ALens' s a  -- ^ lens into the field
        -> g s a

    -- | Boolean field with a default value.
    booleanFieldDef
        :: FieldName     -- ^ field name
        -> ALens' s Bool -- ^ lens into the field
        -> Bool          -- ^ default
        -> g s Bool

    -- | Optional field.
    optionalFieldAla
        :: (Parsec b, Pretty b, Newtype a b)
        => FieldName          -- ^ field name
        -> (a -> b)           -- ^ 'pack'
        -> ALens' s (Maybe a) -- ^ lens into the field
        -> g s (Maybe a)

    -- | Optional field with default value.
    optionalFieldDefAla
        :: (Parsec b, Pretty b, Newtype a b, Eq a)
        => FieldName   -- ^ field name
        -> (a -> b)    -- ^ 'Newtype' pack
        -> ALens' s a  -- ^ @'Lens'' s a@: lens into the field
        -> a           -- ^ default value
        -> g s a

    --  | Free text field is essentially 'optionalFieldDefAla` with @""@
    --  as the default and "accept everything" parser.
    --
    -- @since 3.0.0.0
    freeTextField
        :: FieldName
        -> ALens' s (Maybe String) -- ^ lens into the field
        -> g s (Maybe String)

    --  | Free text field is essentially 'optionalFieldDefAla` with @""@
    --  as the default and "accept everything" parser.
    --
    -- @since 3.0.0.0
    freeTextFieldDef
        :: FieldName
        -> ALens' s String -- ^ lens into the field
        -> g s String

    -- | @since 3.2.0.0
    freeTextFieldDefST
        :: FieldName
        -> ALens' s ShortText -- ^ lens into the field
        -> g s ShortText

    -- | Monoidal field.
    --
    -- Values are combined with 'mappend'.
    --
    -- /Note:/ 'optionalFieldAla' is a @monoidalField@ with 'Last' monoid.
    --
    monoidalFieldAla
        :: (Parsec b, Pretty b, Monoid a, Newtype a b)
        => FieldName   -- ^ field name
        -> (a -> b)    -- ^ 'pack'
        -> ALens' s a  -- ^ lens into the field
        -> g s a

    -- | Parser matching all fields with a name starting with a prefix.
    prefixedFields
        :: FieldName                    -- ^ field name prefix
        -> ALens' s [(String, String)]  -- ^ lens into the field
        -> g s [(String, String)]

    -- | Known field, which we don't parse, neither pretty print.
    knownField :: FieldName -> g s ()

    -- | Field which is parsed but not pretty printed.
    hiddenField :: g s a -> g s a

    -- | Deprecated since
    deprecatedSince
        :: CabalSpecVersion   -- ^ version
        -> String             -- ^ deprecation message
        -> g s a
        -> g s a

    -- | Removed in. If we occur removed field, parsing fails.
    removedIn
        :: CabalSpecVersion   -- ^ version
        -> String             -- ^ removal message
        -> g s a
        -> g s a

    -- | Annotate field with since spec-version.
    availableSince
        :: CabalSpecVersion  -- ^ spec version
        -> a                 -- ^ default value
        -> g s a
        -> g s a

-- | Field which can be defined at most once.
uniqueField
    :: (FieldGrammar g, Parsec a, Pretty a)
    => FieldName   -- ^ field name
    -> ALens' s a  -- ^ lens into the field
    -> g s a
uniqueField fn = uniqueFieldAla fn Identity

-- | Field which can be defined at most once.
optionalField
    :: (FieldGrammar g, Parsec a, Pretty a)
    => FieldName          -- ^ field name
    -> ALens' s (Maybe a) -- ^ lens into the field
    -> g s (Maybe a)
optionalField fn = optionalFieldAla fn Identity

-- | Optional field with default value.
optionalFieldDef
    :: (FieldGrammar g, Functor (g s), Parsec a, Pretty a, Eq a)
    => FieldName   -- ^ field name
    -> ALens' s a  -- ^ @'Lens'' s a@: lens into the field
    -> a           -- ^ default value
    -> g s a
optionalFieldDef fn = optionalFieldDefAla fn Identity

-- | Field which can be define multiple times, and the results are @mappend@ed.
monoidalField
    :: (FieldGrammar g, Parsec a, Pretty a, Monoid a)
    => FieldName   -- ^ field name
    -> ALens' s a  -- ^ lens into the field
    -> g s a
monoidalField fn = monoidalFieldAla fn Identity

-- | Default implementation for 'freeTextFieldDefST'.
defaultFreeTextFieldDefST
    :: (Functor (g s), FieldGrammar g
       , g s @@ String
       )
    => FieldName
    -> ALens' s ShortText -- ^ lens into the field
    -> g s ShortText
defaultFreeTextFieldDefST fn l =
    toShortText <$> freeTextFieldDef fn (cloneLens l . st)
  where
    st :: Lens' ShortText String
    st f s = toShortText <$> f (fromShortText s)



