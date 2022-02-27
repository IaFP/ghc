{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module OpenTF where

-- import GHC.Types 

data Blah a 

type family Elem a
-- type instance Elem [a] = a

foobar :: Elem [a] -> a
foobar = undefined

barfoo :: Elem (Blah a) -> a
barfoo = undefined
