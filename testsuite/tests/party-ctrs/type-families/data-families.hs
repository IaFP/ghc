{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module DataFams where

-- Declare a list-like data family
data family XList a

-- Declare a list-like instance for Char
data instance XList Char = XCons Char (XList Char) | XNil

-- Declare a number-like instance for ()
data instance XList () = XListUnit !Int

-- Should eventually have predicate WF_XList a =>
-- whenever I impement data family support...
g :: XList a -> a
g = undefined
