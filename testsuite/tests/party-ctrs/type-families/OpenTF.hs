{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module OpenTF where

import GHC.Types (Type)

type family Elem a
type instance Elem [a] = a
