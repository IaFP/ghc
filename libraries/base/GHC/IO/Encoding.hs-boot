{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
{-# OPTIONS -fno-enable-rewrite-rules #-}

module GHC.IO.Encoding where

import GHC.IO (IO)
import GHC.IO.Encoding.Types

getLocaleEncoding, getFileSystemEncoding, getForeignEncoding :: IO TextEncoding

