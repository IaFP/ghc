{-# LANGUAGE TypeFamilies, PartialTypeConstructors, UndecidableInstances #-}
module Inn where

import TypeFamilies


type family In a
type instance In (Blah a) = NTF (Blah a)


