{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Completesig15 where
import GHC.Types (type (@@))
class C f where
  foo :: f a -> ()

pattern P :: (C f, f @@ a) => f a
pattern P <- (foo -> ())

{-# COMPLETE P #-}
