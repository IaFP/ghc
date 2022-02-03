{-# LANGUAGE Unsafe, DatatypeContexts #-}
{-# LANGUAGE MagicHash, UnboxedTuples, RoleAnnotations #-}
{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ExplicitNamespaces, TypeOperators, TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE PartialTypeConstructors, UndecidableInstances #-}

module Arr where

import qualified Data.Array.IArray as IA
import qualified Data.Array.Unboxed as UA
import qualified GHC.Arr as GhcArr
import qualified Data.Array.Base as AB

import Data.Int (Int, Int8, Int16, Int32, Int64) 
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Ix (Ix)
import Foreign.StablePtr (StablePtr)
import Foreign.Ptr (Ptr, FunPtr)
import GHC.Show         ( appPrec )
-- import GHC.Types (type (@@))


-- Things that are unboxed
class Unboxed a

instance Unboxed Bool
instance Unboxed Char
instance Unboxed Double
instance Unboxed Float

instance Unboxed Int
instance Unboxed Int8
instance Unboxed Int16
instance Unboxed Int32
instance Unboxed Int64

instance Unboxed Word
instance Unboxed Word8
instance Unboxed Word16
instance Unboxed Word32
instance Unboxed Word64

instance Unboxed (StablePtr a)
instance Unboxed (Ptr a)
instance Unboxed (FunPtr a)


-- Arrays of unboxed elements of type e
newtype (AB.IArray AB.UArray e, Ix i, Unboxed e) => UArray i e = MkUArray (AB.UArray i e)

instance AB.IArray UArray Bool where
  bounds (MkUArray m) = AB.bounds m
  numElements (MkUArray m) = AB.numElements m
  unsafeArray l elms = AB.unsafeArray l elms
  unsafeAt (MkUArray m) l = AB.unsafeAt m l


instance AB.IArray UArray Char where
  bounds (MkUArray m) = AB.bounds m
  numElements (MkUArray m) = AB.numElements m
  unsafeArray l elms = AB.unsafeArray l elms
  unsafeAt (MkUArray m) l = AB.unsafeAt m l

instance AB.IArray UArray Double where
  bounds (MkUArray m) = AB.bounds m
  numElements (MkUArray m) = AB.numElements m
  unsafeArray l elms = AB.unsafeArray l elms
  unsafeAt (MkUArray m) l = AB.unsafeAt m l

instance AB.IArray UArray Float where
  bounds (MkUArray m) = AB.bounds m
  numElements (MkUArray m) = AB.numElements m
  unsafeArray l elms = AB.unsafeArray l elms
  unsafeAt (MkUArray m) l = AB.unsafeAt m l

instance AB.IArray UArray Int where
  bounds (MkUArray m) = AB.bounds m
  numElements (MkUArray m) = AB.numElements m
  unsafeArray l elms = AB.unsafeArray l elms
  unsafeAt (MkUArray m) l = AB.unsafeAt m l

instance AB.IArray UArray Int8 where
  bounds (MkUArray m) = AB.bounds m
  numElements (MkUArray m) = AB.numElements m
  unsafeArray l elms = AB.unsafeArray l elms
  unsafeAt (MkUArray m) l = AB.unsafeAt m l

instance AB.IArray UArray Int16 where
  bounds (MkUArray m) = AB.bounds m
  numElements (MkUArray m) = AB.numElements m
  unsafeArray l elms = AB.unsafeArray l elms
  unsafeAt (MkUArray m) l = AB.unsafeAt m l

instance AB.IArray UArray Int32 where
  bounds (MkUArray m) = AB.bounds m
  numElements (MkUArray m) = AB.numElements m
  unsafeArray l elms = AB.unsafeArray l elms
  unsafeAt (MkUArray m) l = AB.unsafeAt m l

instance AB.IArray UArray Int64 where
  bounds (MkUArray m) = AB.bounds m
  numElements (MkUArray m) = AB.numElements m
  unsafeArray l elms = AB.unsafeArray l elms
  unsafeAt (MkUArray m) l = AB.unsafeAt m l


instance AB.IArray UArray Word where
  bounds (MkUArray m) = AB.bounds m
  numElements (MkUArray m) = AB.numElements m
  unsafeArray l elms = AB.unsafeArray l elms
  unsafeAt (MkUArray m) l = AB.unsafeAt m l

instance AB.IArray UArray Word8 where
  bounds (MkUArray m) = AB.bounds m
  numElements (MkUArray m) = AB.numElements m
  unsafeArray l elms = AB.unsafeArray l elms
  unsafeAt (MkUArray m) l = AB.unsafeAt m l

instance AB.IArray UArray Word16 where
  bounds (MkUArray m) = AB.bounds m
  numElements (MkUArray m) = AB.numElements m
  unsafeArray l elms = AB.unsafeArray l elms
  unsafeAt (MkUArray m) l = AB.unsafeAt m l

instance AB.IArray UArray Word32 where
  bounds (MkUArray m) = AB.bounds m
  numElements (MkUArray m) = AB.numElements m
  unsafeArray l elms = AB.unsafeArray l elms
  unsafeAt (MkUArray m) l = AB.unsafeAt m l

instance AB.IArray UArray Word64 where
  bounds (MkUArray m) = AB.bounds m
  numElements (MkUArray m) = AB.numElements m
  unsafeArray l elms = AB.unsafeArray l elms
  unsafeAt (MkUArray m) l = AB.unsafeAt m l

-- instance IA.IArray UArray String -- This should fail?

-- arr_bad' :: UA.UArray Int String
-- arr_bad' = UA.array (1, 10) [(i, show i) | i <- [1,10]]


uarray :: (i, i) -> [(i, e)] -> UArray i e
uarray bds els = MkUArray $ UA.array bds els

arr_ok :: UArray Int Bool
arr_ok =  uarray (1, 10) [(i, True) | i <- [1,10]]

l_ok :: (Int, Int)
l_ok = IA.bounds arr_ok

arr_char :: UArray Int Char
arr_char = uarray (1, 10) $ take 10 (zip [1,26] ['a'..'z'])

-- arr_bad :: UArray Int String
-- arr_bad = uarray (1, 10) [(i, show i) | i <- [1,10]] -- why doesn't this fail?

-- l_bad :: (Int, Int)
-- l_bad = IA.bounds arr_bad -- fails to typecheck

map'uarray :: (a -> b) -> UArray i a -> UArray i b
map'uarray f (MkUArray a) = MkUArray $ UA.amap f a

instance (Ix i) => Functor (UArray i) where
  fmap = map'uarray

-- arr_bad' :: UArray Int String -- Why doesn't this fail?
-- arr_bad' = fmap show arr_ok

-- l_bad' :: (Int, Int)
-- l_bad' = UA.bounds arr_bad'

-- {-# SPECIALISE
--     showsIArray :: (IA.IArray UArray e, Ix i, Show i, Show e) =>
--                    Int -> UArray i e -> ShowS
--   #-}

showsUArray :: (AB.IArray UArray e, Ix i, Show i, Show e) => Int -> UArray i e -> ShowS
showsUArray p (MkUArray a) =
    showParen (p > appPrec) $
    showString "uarray " .
    shows (AB.bounds a) .
    showChar ' ' .
    shows (AB.assocs a)

-- showsUArray :: (IA.IArray UArray e, Ix i, Show i, Show e) => Int -> UArray i e -> ShowS
-- showsUArray = AB.showsIArray

instance (Ix ix, Show ix, Show e, Unboxed e, AB.IArray UArray e, AB.IArray AB.UArray e) => Show (UArray ix e) where
    showsPrec = showsUArray
