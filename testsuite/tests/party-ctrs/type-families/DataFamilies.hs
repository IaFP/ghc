{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module DataFamilies where

-- Declare a list-like data family
data family List a

-- Declare a list-like instance for Char
data instance List Char = Cons Char (List Char) | Nil

-- Declare a number-like instance for ()
data instance List () = XListUnit !Int

-- Should eventually have predicate WF_XList a =>
-- whenever I impement data family support...
g :: List a -> a
g = undefined
