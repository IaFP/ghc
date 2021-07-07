{-# LANGUAGE PolyKinds #-}

module NewType where


newtype Const a b = Const { getConst :: a }
  deriving (Eq, Ord)


type Errors e = Lift (Const e)

data Lift f a = Pure a | Other (f a)

failure :: e -> Errors e a
failure e = Other (Const e)
