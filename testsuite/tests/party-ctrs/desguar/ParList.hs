{-# LANGUAGE QuantifiedConstraints, MonadComprehensions, ParallelListComp #-}

-- !!! Parallel list comprehensions

module ParList where
import Control.Monad.Zip
import GHC.Base

parlistf :: (Ord b, Num b) => [b] -> [(b, b)]
parlistf xs = [ (x,y) | x <- xs, x>3 | y <- xs ]

main :: IO ()
main = print (parlistf [0..10])

