{-# LANGUAGE MonadComprehensions, ParallelListComp #-}

-- !!! Parallel list comprehensions

module Main where

import GHC.Base (Alternative)
import Control.Monad.Zip (MonadZip)
import GHC.Types (Total)

f :: (Total m, Alternative m, Ord b, Num b, MonadZip m)
  => m b -> m (b, b)
f xs = [ (x,y) | x <- xs, x>3 | y <- xs ]

main = print (f [0..10])

