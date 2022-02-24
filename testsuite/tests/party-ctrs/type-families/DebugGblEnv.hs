{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module DebugGblEnv where

type family F a

g :: F a -> a
g = undefined

