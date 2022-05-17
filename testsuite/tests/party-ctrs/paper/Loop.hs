{-# LANGUAGE QuantifiedConstraints, DataKinds, PolyKinds, RankNTypes, TypeFamilies
    , TypeOperators, UndecidableInstances, ExplicitNamespaces, GeneralisedNewtypeDeriving, DerivingStrategies, DeriveAnyClass #-}

module Loop where


-- type family Loop where
--   Loop = [Loop]

-- class Loopy where 
--   type Loop

-- instance Loopy where
--   type Loop = [Loop]

type family Loop
type instance Loop = [Loop]


ffff :: Loop -> Int
ffff _ = 3

