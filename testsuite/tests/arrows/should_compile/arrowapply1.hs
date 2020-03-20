{-# LANGUAGE Arrows #-}

module ShouldCompile where

import Control.Arrow
import GHC.Types (type (@@))

f :: (Arrow a, a @@ Int, a Int @@ Int) => a (Int,Int,Int) Int
f = proc (x,y,z) -> returnA -< x+y
