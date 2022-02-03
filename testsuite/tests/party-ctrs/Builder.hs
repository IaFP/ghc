{-# LANGUAGE KindSignatures, PolyKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE RankNTypes, FlexibleContexts , ConstrainedClassMethods #-}
{-# LANGUAGE PartialTypeConstructors, QuantifiedConstraints, DatatypeContexts
           , DefaultSignatures, ExistentialQuantification, ConstraintKinds, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass, MagicHash, UnboxedTuples, DataKinds, TypeApplications, UnliftedNewtypes #-}

module ByteString where


-- import GHC.Types (Constraint)
import GHC.Types (type (@@), Type)
import GHC.Exts


data Word8
data ForeignPtr a = ForeignPtr a


data BufferRange = BufferRange (Ptr Word8)  -- First byte of range
                               (Ptr Word8)  -- First byte /after/ range

data ByteString = PS (ForeignPtr Word8) -- payload
                     Int                -- offset
                     Int                -- length

data BuildSignal a =
    Done (Ptr Word8) a
  | BufferFull Int (Ptr Word8) (BuildStep a)
  | InsertChunk (Ptr Word8) ByteString (BuildStep a)

type instance BuildSignal @@ a = ()

type BuildStep a = BufferRange -> IO (BuildSignal a)

newtype Builder = Builder (forall r. BuildStep r -> BuildStep r)

{-# INLINE builder #-}
builder :: forall r. BuildStep r -> BuildStep r
builder = Builder

finalBuildStep :: BuildStep ()
finalBuildStep (BufferRange op _) = return $ Done op ()

ensureFree :: Int -> Builder
ensureFree minFree =
    builder step
  where
    step k br@(BufferRange op ope)
      | ope `minusPtr` op < minFree = return $ bufferFull minFree op k
      | otherwise                   = k br
