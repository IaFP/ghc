{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}

module ShouldCompile where

import Control.Applicative (Applicative)
import GHC.Types (type (@@))

data family S a

newtype instance S Int = S Int
                       deriving Eq

data family S2 a b

type instance S2 Int @@ b = ()
newtype instance S2 Int b = S2 (IO b)
                          deriving (Functor, Applicative, Monad)


