{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
{-# OPTIONS -fno-enable-rewrite-rules #-}

module GHC.IO.Handle.FD where

import GHC.IO.Handle.Types

-- used in GHC.Conc, which is below GHC.IO.Handle.FD
stdout :: Handle

