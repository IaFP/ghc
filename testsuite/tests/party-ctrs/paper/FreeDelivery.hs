{-# LANGUAGE PartialTypeConstructors #-}
{-# LANGUAGE TypeFamilies, QuantifiedConstraints, UndecidableInstances, ExplicitNamespaces #-}

module FreeDelivery where

import GHC.Types (Total, type(@), Type)
import GHC.Base (Applicative (..))

data Command :: Type -> Type where
  Say :: String -> Command ()
  Toast :: Int -> Command ()
  Sense :: () -> Command Int

data (m @ PFree m a, Functor m) => PFree m a = PPure a | PImpure (m (PFree m a))

instance Functor (PFree f) where
  fmap f (PPure a) = PPure $ f a
  fmap f (PImpure mam) = PImpure $ fmap (fmap f) mam


instance Applicative (PFree f) where
  pure = PPure
  liftA2 = undefined
  (*>) = undefined
  (<*) = undefined
  (PPure f) <*> as = fmap f as
  (PImpure faf) <*> as = PImpure (fmap (<*> as) faf)


instance Monad (PFree f) where
   (PPure a) >>= f = f a
   (PImpure x) >>= f = PImpure ((fmap (>>= f)) x)

data Action a = forall r. Action (Command r, r -> a)

instance Functor Action where
  fmap f (Action (c, k)) = Action (c, f. k)


type Program a = PFree Action a

effect :: Command r -> Program r
effect c = do PImpure (Action (c, PPure))

say :: String -> Program ()
say = effect . Say 

toast :: Int -> Program ()
toast = effect . Toast

sense :: Program Int
sense = effect $ Sense ()


straight :: Program ()
straight = do {say "Hello"; toast 3; say "Goodbye"}

branch :: Program ()
branch = do {say "Hello"
              ; t <- sense
              ; if t > 4 then say "hot"
                else do {toast 3; say "Goodbye"}}


data Functor f => FreeA f a where
  APure :: a                       -> FreeA f a
  AMore :: f (b -> a) -> FreeA f b -> FreeA f a


instance Total f => Functor (FreeA f) where
  fmap k (APure a) = APure $ k a
  -- fmap :: (a -> b) -> FreeA f a -> FreeA f b
  -- AMore :: f (c -> a) -> FreeA f c -> FreeA f a
  fmap k (AMore l c) = AMore (fmap (\x -> k . x) l) c


instance Total f => Applicative (FreeA f) where
  pure = APure
  (APure k)   <*> y = fmap k y
  (AMore l c) <*> y = AMore (fmap uncurry l) (pure (,) <*> c <*> y)


type ProgramA a = FreeA Action a

