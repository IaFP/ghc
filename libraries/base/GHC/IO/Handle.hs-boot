{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
{-# OPTIONS -fno-enable-rewrite-rules #-}

module GHC.IO.Handle where

import GHC.IO
import GHC.IO.Handle.Types

hFlush :: Handle -> IO ()

