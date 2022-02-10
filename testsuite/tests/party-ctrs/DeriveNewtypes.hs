

module DeriveNewtypes where

newtype First a = First {getFirst :: Maybe a}
