{-# LANGUAGE PartialTypeConstructors #-}
{-# LANGUAGE TypeFamilies, QuantifiedConstraints, UndecidableInstances, ExplicitNamespaces #-}

module FAM where

import GHC.Types (Total, type(@))

-- data TFree m a = TPure a | TImpure (m (TFree m a))

-- instance (forall a. m @ TFree m a,
--           Monad m) => Functor (TFree m) where
--   fmap :: (TFree m @ a, TFree m @ b) => (a -> b) -> TFree m a -> TFree m b
--   fmap f (TPure a) = TPure $ f a
--   fmap f (TImpure mam) = TImpure $ do x <- mam
--                                       return $ fmap f x

-- data (m @ PFree m a, Monad m) => PFree m a = PPure a | PImpure (m (PFree m a))

-- instance Functor (PFree f) where
--   fmap f (PPure a) = PPure $ f a
--   fmap f (PImpure mam) = PImpure $ do x <- mam
--                                       return $ fmap f x
  

class (forall a. f @ a => f @ t a) => Closed t f
instance (forall a. f @ a => f @ t a) => Closed t f


class (forall a b. (f @ a,  f @ b) => f @ (t a b)) => Closed2 t f 
instance (forall a b. (f @ a,  f @ b) => f @ (t a b)) => Closed2 t f 

-- instance (forall a b. (f @ a, f @ b) => f @ (a -> b))  => Closed2 (->) f
-- instance (forall a b. (f @ a, f @ b) => f @ (a, b)) => Closed (,) f
-- instance (forall a b. (f @ a, f @ b) => f @ (Either a b)) => Closed (Either) f
-- instance (forall a. f @ TFree f a) => Closed (TFree f) f
