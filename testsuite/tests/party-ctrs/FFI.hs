{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE PartialTypeConstructors #-}

module FFI where

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr

type CharOutput = CInt -> IO CInt

foreign import ccall "wrapper" mkCallback :: CharOutput -> IO (FunPtr CharOutput)

