{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications, PolyKinds #-}
{-# LANGUAGE ExistentialQuantification, GADTs, StandaloneDeriving #-}

module DataConElab where
import GHC.Types (Type)
class L a
instance L Int

class R a
instance R Bool

data (Ord a, Eq b) => Either' a b = L a => Left' a | R b => Right' b

data (Ord a, Ord b) => Either'' a b = Left'' a | R b => Right'' b
  deriving (Eq, Ord)

-- Left :: (Eq a, Ord b, L a) => a -> Either' a b

iseqEither :: Either' a b -> Either' a b -> Bool
iseqEither (Left' a1) (Left' a2) = a1 == a2
iseqEither (Right' a1) (Right' a2) = a1 == a2
iseqEither _ _ = False

el1 :: Either' Int Bool
el1 = Left' three

three :: L Int => Int
three = 3


l1 :: Either' Int Bool
l1 = Left' 4

l2 :: Either' Int Bool
l2 = Left' 2


b1, b2 :: Bool
b1 = iseqEither l1 l1
b2 = iseqEither l1 l2

-- cid :: L a => a -> a
-- cid x = id x

-- fid :: L a => a -> a
-- fid a = cid a

newtype SelectT r m a = SelectT ((a -> m r) -> m a)


data Proxy k (a :: k) = P

proxy :: Proxy Type Int
proxy = P

-- y :: Proxy Bool True
-- y = P

-- z :: Proxy Bool 'True
-- z = P


data Wacky k        -- kind              -- NamedTCB Required
           (a :: k) -- constrained type  -- AnnonTCB VisArg
           k'       -- kind              -- NamedTCB Required
           (b ::k') -- constrained type  -- AnnonTCB VisArg
  = W

newtype Const   -- {k1}        -- NamedTCB infered
                -- {k2}        -- NamedTCB infered
              a -- {a :: k1}   -- AnnonTCB VisArg
              b -- {b :: k2}   -- AnnonTCB VisArg
  = Const { getConst :: a }

data N = Z | S N

wacky :: Wacky Type N Type Bool
wacky = W
