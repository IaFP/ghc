{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module DebugGblEnv where

type family F a

a :: F a -> a
a = undefined

b :: F a -> a
b = a

c :: F a -> a
c = undefined

d :: F [a] -> a
d = undefined


