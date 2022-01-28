{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module HigherKinded where


data (Eq a, Eq b) => OR a b = L a | R b

-- Honestly don't know if this is a problem or not.
-- Currently my WF_F kind signature will be wrong.
type family F (a :: *) :: * -> *
type instance F a = OR a

foobar :: F a b -> Bool
foobar (L a) = a == a
foobar (R b) = b == b
