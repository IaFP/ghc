
{-# LANGUAGE DatatypeContexts #-}
-- {-# LANGUAGE PartialTypeConstructors, UndecidableInstances #-}
{-# LANGUAGE RankNTypes, GADTs #-}
{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances, FlexibleContexts, ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor, StandaloneDeriving, FlexibleContexts #-}


module SetM where


import qualified Data.Set as S
import Control.Applicative (Applicative (..))
import GHC.Types

returnSet :: a -> S.Set a
returnSet = S.singleton

bindSet :: (Ord b) => S.Set a -> (a -> S.Set b) -> S.Set b
bindSet s f = S.unions (map f (S.toList s))

data SetM :: Type -> Type where
  ReturnSetM :: a -> SetM a
  BindSetM :: S.Set x -> (x -> SetM a) -> SetM a

instance Show (SetM a) where
  show (ReturnSetM a) = "ReturnSetM"
  show (BindSetM s f) = "BindSetM"

instance Functor SetM where
  fmap f (ReturnSetM a) = ReturnSetM (f a)
  fmap f (BindSetM s g) = BindSetM s (\x -> fmap f $ g x)

instance Applicative SetM where
  pure = ReturnSetM 
  liftA2 _ _ _ = undefined


instance Monad SetM where
  (ReturnSetM a) >>= k = k a
  (BindSetM s f) >>= k = BindSetM s (\x -> f x >>= k)


liftSettoSetM :: S.Set a -> SetM a
liftSettoSetM s = BindSetM s ReturnSetM

lowerSetMtoSet :: Ord a => SetM a -> S.Set a
lowerSetMtoSet (ReturnSetM a) = returnSet a
lowerSetMtoSet (BindSetM s f) = bindSet s (lowerSetMtoSet . f)


sm1 :: SetM (Int, Char)
sm1 = do x1 <- liftSettoSetM (S.fromList [1,2,3,4])
         x2 <- liftSettoSetM (S.fromList ['a', 'b'])
         return (x1, x2)

-- sf1 :: SetM ((Int, Int) -> (Int, Int))
-- sf1 = liftSettoSetM (S.fromList [(1,1),(2,2),(3,3),(4,4)]) >>= return (\ (x, y) (p, q) -> return (x + p, y * q))

-- sm2 :: SetM (Int, Int)
-- sm2 = do g <- sf1
--          y <- liftSettoSetM (S.fromList [(5,4), (6,6), (7,7)])
--          return (g y)

{-

BindSetM sab ReturnSetM >>= \x -> return x

BindSetM sab >>= (\x ->
(ReturnSetM x >>= \x1 ->
(ReturnSetM x1)))

BindSetM sab >>= (\x ->
(\x1 -> (ReturnSetM x1))(ReturnSetM x))

BindSetM sab >>= (\x -> (ReturnSetM (ReturnSetM x)))





S.fromList[1,2,3,4] = s1234
S.fromList[a,b] = sab

BindSet (S.fromList [1,2,3,4]) ReturnSetM
>>= \x1 -> BindSet (S.fromList ['a','b']) ReturnSetM
>>= \x2 -> return (x1, x2)

======

BindSetM s1234 ReturnSetM >>= (\x1 ->
   BindSetM sab ReturnSetM >>= (\x2 ->
         return (x1, x2)))

=====

BindSetM s1234 ReturnSetM >>= (\x1 ->
   BindSetM sab ReturnSetM >>= (\x2 ->
         ReturnSetM (x1, x2)))

=====

BindSetM s1234 ReturnSetM >>= (\x1 ->
   BindSetM sab >>= (\x -> ReturnSetM x >>= (\x2 ->
         ReturnSetM (x1, x2))))

=====
BindSetM s1234 ReturnSetM >>= (\x1 ->
   BindSetM sab >>= (\x -> (\x2 ->
         ReturnSetM (x1, x2)) (ReturnSetM x)))

=====

BindSetM s1234 ReturnSetM >>= (\x1 ->
    BindSetM sab >>= (\x -> (\x2 ->
         ReturnSetM (x1, ReturnSetM x))))

=====

BindSetM s1234 >>= (\y -> \x1 ->
    BindSetM sab >>= (\x -> (\x2 ->
         ReturnSetM (x1, ReturnSetM x))))

=====

-}
