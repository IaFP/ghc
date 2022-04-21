{-# LANGUAGE MonadComprehensions, ParallelListComp #-}

-- !!! Parallel list comprehensions

module Main where

f :: (Ord b, Num b) => [b] -> [(b, b)]
f xs = [ (x,y) | x <- xs, x>3 | y <- xs ]

main = print (f [0..10])

