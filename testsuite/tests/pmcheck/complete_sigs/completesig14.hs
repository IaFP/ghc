{-# OPTIONS_GHC -Wall #-}
module Completesig11 where

data A = A | B

{-#┬áCOMPLETE A, B #-}

foo :: A -> ()
foo A = ()
foo B = ()
foo A = ()
