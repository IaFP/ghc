{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Graph  where

import GHC.Types (WFT)

{--------------------------------------------------------------------------------
Test Case from libraries/Cabal/Cabal/src/Distribution/Compat/Graph.hs
=====================================================================

--------------------------------------------------------------------------------}
class Ord (Key a) => IsNode a where
    type Key a
    nodeKey :: a -> Key a
    nodeNeighbors :: a -> [Key a]

data WFT (Key a) => Graph a = UndefinedG
data Map k v = UndefinedM

toMap :: Graph a -> Map (Key a) a
toMap = undefined

elems :: Map k v -> [v]
elems = undefined

toList :: Graph a -> [a]
toList g = elems (toMap g)

