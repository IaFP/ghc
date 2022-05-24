{-# LANGUAGE MagicHash, NoImplicitPrelude, TypeFamilies, UnboxedTuples,
             MultiParamTypeClasses, RoleAnnotations, CPP, TypeOperators,
             PolyKinds, NegativeLiterals, DataKinds, ScopedTypeVariables,
             TypeApplications, StandaloneKindSignatures,
             FlexibleInstances, UndecidableInstances,
             UndecidableSuperClasses, RankNTypes, ConstraintKinds, QuantifiedConstraints #-}
-- NegativeLiterals: see Note [Fixity of (->)]
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Types
-- Copyright   :  (c) The University of Glasgow 2009
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC type definitions.
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Types (
        -- Data types that are built-in syntax
        -- They are defined here, but not explicitly exported
        --
        --    Lists:          []( [], (:) )
        --    Type equality:  (~)( Eq# )

        Bool(..), Char(..), Int(..), Word(..),
        Float(..), Double(..),
        Ordering(..), IO(..),
        isTrue#,
        SPEC(..),
        Symbol,
        Any,
        type (~~), Coercible,
        TYPE, Levity(..), RuntimeRep(..),
        LiftedRep, UnliftedRep,
        -- Things for representing well-defined constraints
        type (@), WDT, Total, Total2,
        -- Things that are Unboxable
        -- Int, Double, Float etc. and have a bare machine representation and not via a pointer
        Unboxable,

        Type, UnliftedType, Constraint,
          -- The historical type * should ideally be written as
          -- `type *`, without the parentheses. But that's a true
          -- pain to parse, and for little gain.
        ZeroBitRep, ZeroBitType,
        VecCount(..), VecElem(..),
        Void#,

        -- * Runtime type representation
        Module(..), TrName(..), TyCon(..), TypeLitSort(..),
        KindRep(..), KindBndr,

        -- * Multiplicity Types
        Multiplicity(..), MultMul
    ) where

import GHC.Prim

infixr 5 :


{- *********************************************************************
*                                                                      *
                  Functions
*                                                                      *
********************************************************************* -}

infixr -1 ->
{-
Note [Fixity of (->)]
~~~~~~~~~~~~~~~~~~~~~
This declaration is important for :info (->) command (issue #10145)
1) The parser parses -> as if it had lower fixity than 0,
   so we conventionally use -1 (issue #15235).
2) Fixities outside the 0-9 range are exceptionally allowed
   for (->) (see checkPrecP in RdrHsSyn)
3) The negative fixity -1 must be parsed as a single token,
   hence this module requires NegativeLiterals.
-}

-- | The regular function type
type (->) = FUN 'Many
-- See Note [Linear Types] in Multiplicity

{- *********************************************************************
*                                                                      *
                  Kinds
*                                                                      *
********************************************************************* -}

-- | The kind of constraints, like @Show a@
data Constraint

-- | The runtime representation of lifted types.
type LiftedRep = 'BoxedRep 'Lifted

-- | The runtime representation of unlifted types.
type UnliftedRep = 'BoxedRep 'Unlifted

-- | The runtime representation of a zero-width tuple,
--   represented by no bits at all
type ZeroBitRep = 'TupleRep '[]

-------------------------
-- | The kind of types with lifted values. For example @Int :: Type@.
type Type = TYPE LiftedRep

-- | The kind of boxed, unlifted values, for example @Array#@ or a user-defined
-- unlifted data type, using @-XUnliftedDataTypes@.
type UnliftedType = TYPE UnliftedRep

-- | The kind of the empty unboxed tuple type (# #)
type ZeroBitType = TYPE ZeroBitRep

-------------------------
data Multiplicity = Many | One

type family MultMul (a :: Multiplicity) (b :: Multiplicity) :: Multiplicity where
  MultMul 'One x = x
  MultMul x 'One = x
  MultMul 'Many x = 'Many
  MultMul x 'Many = 'Many

{- *********************************************************************
*                                                                      *
                  Symbol
*                                                                      *
********************************************************************* -}

-- | (Kind) This is the kind of type-level symbols.
-- Declared here because class IP needs it
data Symbol

{- *********************************************************************
*                                                                      *
                  Any
*                                                                      *
********************************************************************* -}

-- | The type constructor 'Any' is type to which you can unsafely coerce any
-- lifted type, and back. More concretely, for a lifted type @t@ and
-- value @x :: t@, -- @unsafeCoerce (unsafeCoerce x :: Any) :: t@ is equivalent
-- to @x@.
--
type family Any :: k where { }
-- See Note [Any types] in GHC.Builtin.Types. Also, for a bit of history on Any see
-- #10886. Note that this must be a *closed* type family: we need to ensure
-- that this can't reduce to a `data` type for the results discussed in
-- Note [Any types].

{- *********************************************************************
*                                                                      *
                  Lists

   NB: lists are built-in syntax, and hence not explicitly exported
*                                                                      *
********************************************************************* -}

-- | The builtin list type, usually written in its non-prefix form @[a]@.
--
-- ==== __Examples__
--
-- Unless the OverloadedLists extension is enabled, list literals are
-- syntatic sugar for repeated applications of @:@ and @[]@.
--
-- >>> 1:2:3:4:[] == [1,2,3,4]
-- True
--
-- Similarly, unless the OverloadedStrings extension is enabled, string
-- literals are syntactic sugar for a lists of characters.
--
-- >>> ['h','e','l','l','o'] == "hello"
-- True
--
data [] a = [] | a : [a]


{- *********************************************************************
*                                                                      *
                  Ordering
*                                                                      *
********************************************************************* -}

data Ordering = LT | EQ | GT


{- *********************************************************************
*                                                                      *
                  Int, Char, Word, Float, Double
*                                                                      *
********************************************************************* -}

{- | The character type 'Char' is an enumeration whose values represent
Unicode (or equivalently ISO\/IEC 10646) code points (i.e. characters, see
<http://www.unicode.org/> for details).  This set extends the ISO 8859-1
(Latin-1) character set (the first 256 characters), which is itself an extension
of the ASCII character set (the first 128 characters).  A character literal in
Haskell has type 'Char'.

To convert a 'Char' to or from the corresponding 'Int' value defined
by Unicode, use 'Prelude.toEnum' and 'Prelude.fromEnum' from the
'Prelude.Enum' class respectively (or equivalently 'Data.Char.ord' and
'Data.Char.chr').
-}
data {-# CTYPE "HsChar" #-} Char = C# Char#

-- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded' class.
data {-# CTYPE "HsInt" #-} Int = I# Int#

-- |A 'Word' is an unsigned integral type, with the same size as 'Int'.
data {-# CTYPE "HsWord" #-} Word = W# Word#

-- | Single-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE single-precision type.
data {-# CTYPE "HsFloat" #-} Float = F# Float#

-- | Double-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE double-precision type.
data {-# CTYPE "HsDouble" #-} Double = D# Double#


{- *********************************************************************
*                                                                      *
                    IO
*                                                                      *
********************************************************************* -}

{- |
A value of type @'IO' a@ is a computation which, when performed,
does some I\/O before returning a value of type @a@.

There is really only one way to \"perform\" an I\/O action: bind it to
@Main.main@ in your program.  When your program is run, the I\/O will
be performed.  It isn't possible to perform I\/O from an arbitrary
function, unless that function is itself in the 'IO' monad and called
at some point, directly or indirectly, from @Main.main@.

'IO' is a monad, so 'IO' actions can be combined using either the do-notation
or the 'Prelude.>>' and 'Prelude.>>=' operations from the 'Prelude.Monad'
class.
-}
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))


{- *********************************************************************
*                                                                      *
                    (~) and Coercible

   NB: (~) is built-in syntax, and hence not explicitly exported
*                                                                      *
********************************************************************* -}

{-
Note [Kind-changing of (~) and Coercible]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(~) and Coercible are tricky to define. To the user, they must appear as
constraints, but we cannot define them as such in Haskell. But we also cannot
just define them only in GHC.Prim (like (->)), because we need a real module
for them, e.g. to compile the constructor's info table.

Furthermore the type of MkCoercible cannot be written in Haskell
(no syntax for ~#R).

So we define them as regular data types in GHC.Types, and do magic in GHC.Builtin.Types,
inside GHC, to change the kind and type.
-}


-- | Lifted, heterogeneous equality. By lifted, we mean that it
-- can be bogus (deferred type error). By heterogeneous, the two
-- types @a@ and @b@ might have different kinds. Because @~~@ can
-- appear unexpectedly in error messages to users who do not care
-- about the difference between heterogeneous equality @~~@ and
-- homogeneous equality @~@, this is printed as @~@ unless
-- @-fprint-equality-relations@ is set.
--
-- In @0.7.0@, the fixity was set to @infix 4@ to match the fixity of 'Data.Type.Equality.:~~:'.
class a ~~ b

  -- See also Note [The equality types story] in GHC.Builtin.Types.Prim

-- | Lifted, homogeneous equality. By lifted, we mean that it
-- can be bogus (deferred type error). By homogeneous, the two
-- types @a@ and @b@ must have the same kinds.

-- In @0.7.0@, the fixity was set to @infix 4@ to match the fixity of 'Data.Type.Equality.:~:'.
class a ~ b

infix 4 ~, ~~
  -- See also Note [The equality types story] in GHC.Builtin.Types.Prim

-- | @Coercible@ is a two-parameter class that has instances for types @a@ and @b@ if
--      the compiler can infer that they have the same representation. This class
--      does not have regular instances; instead they are created on-the-fly during
--      type-checking. Trying to manually declare an instance of @Coercible@
--      is an error.
--
--      Nevertheless one can pretend that the following three kinds of instances
--      exist. First, as a trivial base-case:
--
--      @instance Coercible a a@
--
--      Furthermore, for every type constructor there is
--      an instance that allows to coerce under the type constructor. For
--      example, let @D@ be a prototypical type constructor (@data@ or
--      @newtype@) with three type arguments, which have roles @nominal@,
--      @representational@ resp. @phantom@. Then there is an instance of
--      the form
--
--      @instance Coercible b b\' => Coercible (D a b c) (D a b\' c\')@
--
--      Note that the @nominal@ type arguments are equal, the
--      @representational@ type arguments can differ, but need to have a
--      @Coercible@ instance themself, and the @phantom@ type arguments can be
--      changed arbitrarily.
--
--      The third kind of instance exists for every @newtype NT = MkNT T@ and
--      comes in two variants, namely
--
--      @instance Coercible a T => Coercible a NT@
--
--      @instance Coercible T b => Coercible NT b@
--
--      This instance is only usable if the constructor @MkNT@ is in scope.
--
--      If, as a library author of a type constructor like @Set a@, you
--      want to prevent a user of your module to write
--      @coerce :: Set T -> Set NT@,
--      you need to set the role of @Set@\'s type parameter to @nominal@,
--      by writing
--
--      @type role Set nominal@
--
--      For more details about this feature, please refer to
--      <http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/coercible.pdf Safe Coercions>
--      by Joachim Breitner, Richard A. Eisenberg, Simon Peyton Jones and Stephanie Weirich.
--
--      @since 4.7.0.0
class Coercible (a :: k) (b :: k)
  -- See also Note [The equality types story] in GHC.Builtin.Types.Prim

{- *********************************************************************
*                                                                      *
                    (@@) and TyAt (Wellformed)

*                                                                      *
********************************************************************* -}
-- |    The predicate that asserts type application is sound
--      This is an implimentation of partial type constructor
--      Mark P. Jones, J. Garrett Morris, and Richard A. Eisenberg. 2019.
--      Partial type constructors: or, making ad hoc datatypes less ad hoc.
--      Proc. ACM Program. Lang. 4, POPL, Article 40 (January 2020).
--      DOI: <https://dl.acm.org/doi/10.1145/3371108>

type family (@) (t :: k' -> k) (u :: k') :: Constraint

-- type WDT t = t ~ t
class WDT a
instance WDT a

-- type Total a = forall a. f @ a -- ideally but can't do this.
class f @ a => Cheat (f :: k' -> k) (a :: k')
instance f @ a => Cheat (f :: k' -> k) (a :: k')

class (f @ a, f a @ b) => Cheat2 (f::k''-> k' -> k) (a::k'') (b:: k')
instance (f @ a, f a @ b) => Cheat2 (f::k''-> k' -> k) (a::k'') (b:: k')


type Total (f::k' -> k) = forall (a:: k'). Cheat f a
type Total2 (f:: k'' -> k' -> k) = forall (a::k'') (b::k'). Cheat2 f a b

-- Things that aren't represented as a pointer
class Unboxable a
instance Unboxable Bool
instance Unboxable Char
instance Unboxable Int
instance Unboxable Word
instance Unboxable Double
instance Unboxable Float


{- *********************************************************************
*                                                                      *
                   Bool, and isTrue#
*                                                                      *
********************************************************************* -}

data {-# CTYPE "HsBool" #-} Bool = False | True

{-# INLINE isTrue# #-}
-- | Alias for 'tagToEnum#'. Returns True if its parameter is 1# and False
--   if it is 0#.
isTrue# :: Int# -> Bool   -- See Note [Optimizing isTrue#]
isTrue# x = tagToEnum# x

{- Note [Optimizing isTrue#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Current definition of isTrue# is a temporary workaround. We would like to
have functions isTrue# and isFalse# defined like this:

    isTrue# :: Int# -> Bool
    isTrue# 1# = True
    isTrue# _  = False

    isFalse# :: Int# -> Bool
    isFalse# 0# = True
    isFalse# _  = False

These functions would allow us to safely check if a tag can represent True
or False. Using isTrue# and isFalse# as defined above will not introduce
additional case into the code. When we scrutinize return value of isTrue#
or isFalse#, either explicitly in a case expression or implicitly in a guard,
the result will always be a single case expression (given that optimizations
are turned on). This results from case-of-case transformation. Consider this
code (this is both valid Haskell and Core):

case isTrue# (a ># b) of
    True  -> e1
    False -> e2

Inlining isTrue# gives:

case (case (a ># b) of { 1# -> True; _ -> False } ) of
    True  -> e1
    False -> e2

Case-of-case transforms that to:

case (a ># b) of
  1# -> case True of
          True  -> e1
          False -> e2
  _  -> case False of
          True  -> e1
          False -> e2

Which is then simplified by case-of-known-constructor:

case (a ># b) of
  1# -> e1
  _  -> e2

While we get good Core here, the code generator will generate very bad Cmm
if e1 or e2 do allocation. It will push heap checks into case alternatives
which results in about 2.5% increase in code size. Until this is improved we
just make isTrue# an alias to tagToEnum#. This is a temporary solution (if
you're reading this in 2023 then things went wrong). See #8326.
-}


{- *********************************************************************
*                                                                      *
                    SPEC
*                                                                      *
********************************************************************* -}

-- | 'SPEC' is used by GHC in the @SpecConstr@ pass in order to inform
-- the compiler when to be particularly aggressive. In particular, it
-- tells GHC to specialize regardless of size or the number of
-- specializations. However, not all loops fall into this category.
--
-- Libraries can specify this by using 'SPEC' data type to inform which
-- loops should be aggressively specialized.
data SPEC = SPEC | SPEC2


{- *********************************************************************
*                                                                      *
                    Levity polymorphism
*                                                                      *
********************************************************************* -}

-- | Whether a boxed type is lifted or unlifted.
data Levity = Lifted | Unlifted

-- | GHC maintains a property that the kind of all inhabited types
-- (as distinct from type constructors or type-level data) tells us
-- the runtime representation of values of that type. This datatype
-- encodes the choice of runtime value.
-- Note that 'TYPE' is parameterised by 'RuntimeRep'; this is precisely
-- what we mean by the fact that a type's kind encodes the runtime
-- representation.
--
-- For boxed values (that is, values that are represented by a pointer),
-- a further distinction is made, between lifted types (that contain ⊥),
-- and unlifted ones (that don't).
data RuntimeRep = VecRep VecCount VecElem   -- ^ a SIMD vector type
                | TupleRep [RuntimeRep]     -- ^ An unboxed tuple of the given reps
                | SumRep [RuntimeRep]       -- ^ An unboxed sum of the given reps
                | BoxedRep Levity -- ^ boxed; represented by a pointer
                | IntRep          -- ^ signed, word-sized value
                | Int8Rep         -- ^ signed,  8-bit value
                | Int16Rep        -- ^ signed, 16-bit value
                | Int32Rep        -- ^ signed, 32-bit value
                | Int64Rep        -- ^ signed, 64-bit value (on 32-bit only)
                | WordRep         -- ^ unsigned, word-sized value
                | Word8Rep        -- ^ unsigned,  8-bit value
                | Word16Rep       -- ^ unsigned, 16-bit value
                | Word32Rep       -- ^ unsigned, 32-bit value
                | Word64Rep       -- ^ unsigned, 64-bit value (on 32-bit only)
                | AddrRep         -- ^ A pointer, but /not/ to a Haskell value
                | FloatRep        -- ^ a 32-bit floating point number
                | DoubleRep       -- ^ a 64-bit floating point number

-- RuntimeRep is intimately tied to TyCon.RuntimeRep (in GHC proper). See
-- Note [RuntimeRep and PrimRep] in RepType.
-- See also Note [Wiring in RuntimeRep] in GHC.Builtin.Types
-- See also Note [TYPE and RuntimeRep] in GHC.Builtin.Type.Prim

-- | Length of a SIMD vector type
data VecCount = Vec2
              | Vec4
              | Vec8
              | Vec16
              | Vec32
              | Vec64
-- Enum, Bounded instances in GHC.Enum

-- | Element of a SIMD vector type
data VecElem = Int8ElemRep
             | Int16ElemRep
             | Int32ElemRep
             | Int64ElemRep
             | Word8ElemRep
             | Word16ElemRep
             | Word32ElemRep
             | Word64ElemRep
             | FloatElemRep
             | DoubleElemRep
-- Enum, Bounded instances in GHC.Enum

{-# DEPRECATED Void# "Void# is now an alias for the unboxed tuple (# #)." #-}
type Void# = (# #)

{- *********************************************************************
*                                                                      *
             Runtime representation of TyCon
*                                                                      *
********************************************************************* -}

{- Note [Runtime representation of modules and tycons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We generate a binding for M.$modName and M.$tcT for every module M and
data type T.  Things to think about

  - We want them to be economical on space; ideally pure data with no thunks.

  - We do this for every module (except this module GHC.Types), so we can't
    depend on anything else (eg string unpacking code)

That's why we have these terribly low-level representations.  The TrName
type lets us use the TrNameS constructor when allocating static data;
but we also need TrNameD for the case where we are deserialising a TyCon
or Module (for example when deserialising a TypeRep), in which case we
can't conveniently come up with an Addr#.
-}

#include "MachDeps.h"

data Module = Module
                TrName   -- ^ Package name
                TrName   -- ^ Module name

data TrName
  = TrNameS Addr#  -- ^ Static
  | TrNameD [Char] -- ^ Dynamic

-- | A de Bruijn index for a binder within a 'KindRep'.
type KindBndr = Int

-- | The representation produced by GHC for conjuring up the kind of a
-- 'Data.Typeable.TypeRep'.

-- See Note [Representing TyCon kinds: KindRep] in GHC.Tc.Instance.Typeable.
data KindRep = KindRepTyConApp TyCon [KindRep]
             | KindRepVar !KindBndr
             | KindRepApp KindRep KindRep
             | KindRepFun KindRep KindRep
             | KindRepTYPE !RuntimeRep
             | KindRepTypeLitS TypeLitSort Addr#
             | KindRepTypeLitD TypeLitSort [Char]

data TypeLitSort = TypeLitSymbol
                 | TypeLitNat
                 | TypeLitChar

-- Show instance for TyCon found in GHC.Show
data TyCon = TyCon Word64#    -- ^ Fingerprint (high)
                   Word64#    -- ^ Fingerprint (low)
                   Module     -- ^ Module in which this is defined
                   TrName     -- ^ Type constructor name
                   Int#       -- ^ How many kind variables do we accept?
                   KindRep    -- ^ A representation of the type's kind
