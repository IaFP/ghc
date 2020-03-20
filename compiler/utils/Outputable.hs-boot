{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators #-}
#endif
module Outputable where

import GhcPrelude
import GHC.Stack( HasCallStack )

data SDoc

showSDocUnsafe :: SDoc -> String

warnPprTrace :: HasCallStack => Bool -> String -> Int -> SDoc -> a -> a

text :: String -> SDoc
