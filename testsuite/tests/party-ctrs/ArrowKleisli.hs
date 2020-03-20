{-# LANGUAGE PartialTypeConstructors
           , DatatypeContexts
           , DefaultSignatures
           , TypeOperators
           , TypeFamilies
           , ExplicitNamespaces  #-}
{-# LANGUAGE DeriveAnyClass
           , MagicHash
           , DataKinds
           , TypeApplications
           , UnliftedNewtypes
           , RankNTypes
           , StandaloneDeriving
           , DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}

module ArrowKleisli where

import GHC.Types (type (@@), Total, Total2)
import qualified GHC.Base as B(id,(.))
import Data.Type.Coercion
import Data.Type.Equality
import Data.Coerce (coerce)


import Partial10

infixr 9 .
infixr 1 >>>, <<<

infixr 3 ***
infixr 3 &&&

  
type instance a @@ () = ()


class Category cat where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

instance Category (->) where
  id = B.id
  (.) = (B..)

{-# RULES
"identity/left" forall p .
                ArrowKleisli.id ArrowKleisli.. p = p
"identity/right"        forall p .
                p ArrowKleisli.. ArrowKleisli.id = p
-- "association"   forall p q r .
--                  (p ArrowKleisli.. q) ArrowKleisli.. r = p ArrowKleisli.. (q ArrowKleisli.. r)
 #-}


(<<<) :: Category cat => cat b c -> cat a b -> cat a c
(<<<) = (ArrowKleisli..)

(>>>) :: Category cat => cat a b -> cat b c -> cat a c
f >>> g = g ArrowKleisli.. f


class Category a => Arrow a where
  {-# MINIMAL arr, (first | (***)) #-}
  arr :: (b -> c) -> a b c

  first :: a b c -> a (b,d) (c,d)
  default first :: (a @@ d, a d @@ d, a (d, c) @@ (c, d), a @@ (d, c)
           , a (d, c) @@ (d, c), a @@ c, a c @@ c, a (c, d) @@ (d, c)
           , a (c, d) @@ (c, d), a @@ (c, d)) =>
    a b c -> a (b,d) (c,d)
  first = (*** ArrowKleisli.id)

  second :: a b c -> a (d,b) (d,c)
  default second :: (a @@ d, a d @@ d
            , a (b, d) @@ (d, c), a (c, d) @@ (d, c)
            , a @@ (c, d), a (b, d) @@ (c, d), a @@ (b, d), a (d, b) @@ (b, d)
            , a (d, b) @@ (d, b), a b @@ b
            , a (d, c) @@ (c, d), a @@ (d, c)
            , a (d, c) @@ (d, c), a @@ c, a c @@ c, a (c, d) @@ (c, d)
            , a (b, d) @@ (d, b), a (b, d) @@ (b, d))
         => a b c -> a (d,b) (d,c)
  second = (ArrowKleisli.id ***)

  (***) ::  a b c -> a b' c' -> a (b,b') (c,c')
  default (***) :: (a b' @@ b', a (b, b') @@ (c, b')
           , a @@ (c, b'), a (c, b') @@ (c, c')
           , a (c, b') @@ (b', c), a @@ (b', c)
           , a (b, b') @@ (c, c'), a @@ (b, b')
           , a c @@ c, a @@ c, a (b', c) @@ (c', c)
           , a @@ (c', c), a (c', c) @@ (c, c')
           , a (b', c) @@ (c, c')
           , a (c, b') @@ (c, b'), a (b', c) @@ (b', c)
           , a (b', c) @@ (c, b'), a (c', c) @@ (c', c)
           , a c' @@ c', a @@ c'
           , a (c, c') @@ (c, c'), a @@ (c, c'), a (c, c') @@ (c', c))
        =>  a b c -> a b' c' -> a (b,b') (c,c')
  f *** g = first f >>> arr swap >>> first g >>> arr swap
    where swap ~(x,y) = (y,x)
  
  (&&&) :: a b c -> a b c' -> a b (c,c')
  default (&&&) :: (a b @@ (b, b), a @@ (b, b)
           , a (b, b) @@ (c, c')
           , a (b, c) @@ (c, c'), a (c', c) @@ (c, c')
           , a @@ (c', c), a (b, c) @@ (c', c), a @@ c, a c @@ c, a @@ (b, c)
           , a (c, b) @@ (b, c), a (c, b) @@ (c, c'), a @@ (c, b)
           , a (b, b) @@ (c, b), a b @@ b
           , a (c, c') @@ (c', c), a @@ (c, c')
           , a (c, c') @@ (c, c'), a @@ c', a c' @@ c', a (c', c) @@ (c', c)
           , a (b, c) @@ (c, b), a (b, c) @@ (b, c), a (c, b) @@ (c, b))
        =>
    a b c -> a b c' -> a b (c,c')
  f &&& g = arr (\z -> (z,z)) >>> f *** g


data Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Functor' m => Functor' (Kleisli m a) where
  fmap' f (Kleisli a) = Kleisli (\ q -> fmap' f (a q))

instance Applicative' m => Applicative' (Kleisli m a) where
  pure' = Kleisli B.. const' B.. pure'
  liftA2' f x = (<<*>>) (fmap' f x)

instance Alternative' m => Alternative' (Kleisli m a) where
  empty' = Kleisli $ const' empty'
  Kleisli f <<|>> Kleisli g = Kleisli $ \x -> f x <<|>> g x

instance Monad' m => Monad' (Kleisli m a) where
  bind' (Kleisli f) k = Kleisli $ \x -> bind' (f x) (\a -> runKleisli (k a) x)

instance MonadPlus' m => MonadPlus' (Kleisli m a) where
  mzero' = Kleisli $ const' mzero'
  Kleisli f `mplus'` Kleisli g = Kleisli $ \x -> f x `mplus'` g x

instance Monad' m => Category (Kleisli m) where
  id = Kleisli return'
  (Kleisli f) . (Kleisli g) = Kleisli (\b -> bind' (g b) f)

instance Monad' m => Arrow (Kleisli m) where
    arr f = Kleisli (return' ArrowKleisli.. f)
    first (Kleisli f) = Kleisli (\ ~(b,d) -> bind' (f b) (\c -> return' (c,d)))
    second (Kleisli f) = Kleisli (\ ~(d,b) -> bind' (f b) (\c -> return' (d,c)))
    -- f *** g = first f >>> arr swap >>> first g >>> arr swap
    --   where swap ~(x,y) = (y,x)
    f &&& g = arr (\z -> (z,z)) >>> f *** g
