{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Graph  where

{--------------------------------------------------------------------------------
Test Case from libraries/Cabal/Cabal/src/Distribution/Compat/Graph.hs
=====================================================================

Work on Closed TFs broke this code (even though it's an associated TF).

--------------------------------------------------------------------------------}
class Ord (Key a) => IsNode a where
    type Key a
    nodeKey :: a -> Key a
    nodeNeighbors :: a -> [Key a]

-- Not bothering with actual definitions.
data Graph a = UndefinedG
data Map k v = UndefinedM

toMap :: Graph a -> Map (Key a) a
toMap = undefined

elems :: Map k v -> [v]
elems = undefined

-- This function breaks because
-- `toMap` requires `$wf'Key a`, which
-- is (as a bug) not in the constraints of toList.
toList :: Graph a -> [a]
toList g = elems (toMap g)

