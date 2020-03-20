
{-# LANGUAGE MonadComprehensions, ParallelListComp #-}

module Foo where

import Control.Monad.Zip
import GHC.Types (Total)

foo :: (MonadZip m, Total m) => m ()
foo = [ ()
      | () <- foo
      | () <- foo
      ]

