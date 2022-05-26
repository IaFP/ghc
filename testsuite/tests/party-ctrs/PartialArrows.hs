{-# LANGUAGE PartialTypeConstructors #-}
{-# LANGUAGE TypeFamilies, QuantifiedConstraints, UndecidableInstances, ExplicitNamespaces #-}

module PartialArrows where

import GHC.Types (type(@))
  
{- Normaly arrows are from arbitrary types to arbitrary types.
   This makes them too powerful to be used to model function execution on a remote machine
   with a limited memory.

   What if we have an arrow that is constrained in its type
-}
class (forall a. f @ a => f @ t a) => Closed t f
instance (forall a. f @ a => f @ t a) => Closed t f


class (forall a b. (f @ a,  f @ b) => f @ (t a b)) => Closed2 t f 
instance (forall a b. (f @ a,  f @ b) => f @ (t a b)) => Closed2 t f 


type CloseBin t = (Closed2 t (,), Closed2 t Either, Closed2 t (->))

class Ord a => Finite a where
  enumerate :: [a]
  minF :: a
  maxF :: a

instance Finite Bool where
  enumerate = [True, False]
  minF = False
  maxF = True

instance (Finite a, Finite b) => Finite (a, b) -- where
  -- enumerate = [(a, b) | a <- enumerate, b <- enumerate]
  -- minF = undefined
  -- maxF = undefined
instance (Finite a, Finite b) => Finite (Either a b)

infix 1 :->
newtype -- (Finite a, Finite b) => 
  a :-> b = MkFArr { unArrow :: a -> b }

{-
This has an identity and is also composable 
-}

idFArr :: a :-> a
idFArr = MkFArr {unArrow = id}

compFArr :: (b :-> c) -> (a :-> b) -> (a :-> c)
compFArr (MkFArr f) (MkFArr g) =  MkFArr (f . g)

class FArrow a where
  arr :: (b :-> c) -> a b c
  (>>>) :: a b c -> a c d -> a b d
  first :: a b c -> a (b, d) (c, d)
  second :: a b c -> a (d, b) (d, c)

  (***) :: a b c -> a d e ->  a (b, d) (c, e)
  (&&&) :: a b c -> a b d ->  a b (c, d)
  
newtype (-- Finite a, Finite (m b)
         m @ b ) => FKleisli m a b = FK (a :-> m b)

instance Monad m => FArrow (FKleisli m) where
  arr f = FK (MkFArr $ \b -> return $ (unArrow f) b)
  FK f >>> FK g = FK (MkFArr $ \b -> f' b >>= g')
    where f' = unArrow f
          g' = unArrow g

  first (FK f) = FK (MkFArr $ \ (b, d) -> do c <- f' b
                                             return (c, d))
    where f' = unArrow f

  second (FK f) = FK (MkFArr $ \ (d, b) -> do c <- f' b
                                              return (d, c))
    where f' = unArrow f

  (FK f) *** (FK g) = FK (MkFArr $ \ (b, d) -> do c <- f' b
                                                  e <- g' d
                                                  return (c, e))
    where f' = unArrow f
          g' = unArrow g

  (FK f) &&& (FK g) = FK (MkFArr $ \ b -> do c <- f' b
                                             e <- g' b
                                             return (c, e))
    where f' = unArrow f
          g' = unArrow g


instance FArrow (:->) where
  arr f = f
  (MkFArr f) >>> (MkFArr g) = MkFArr (g . f)
  first (MkFArr f) = MkFArr $ \ (b, d) -> (f b, d)
  second (MkFArr f) = MkFArr $ \ (b, d) -> (b, f d)
  -- first = (*** idFArr)
  -- second = (idFArr ***)

  (MkFArr f) *** (MkFArr g) = MkFArr $ \(b,d) -> (f b, g d)
  (MkFArr f) &&& (MkFArr g) = MkFArr $ \b -> (f b, g b)


second' :: forall a b c d. ( (forall x y. (a @ x, a @ y) => a @ (x, y))
           -- , (forall x. (a @ x) => a @ (x, x))
                           , (forall x y. (a @ (x, y)) => a @ (y, x))
                           , FArrow a) => a b c -> a (d, b) (d, c)
second' f = arr fswap >>> first f >>> arr fswap
  where fswap :: (a, b) :-> (b, a)
        fswap = MkFArr $ \(a,b) -> (b,a)
-- (***) :: FArrow a => a b c -> a d e ->  a (b, d) (c, e)
-- f *** g = first f >>> second g

class FArrow a => FArrowZero a where
  zeroFArrow :: a b c


class FArrowZero a => FArrowPlus a where
  (<+>) :: a b c -> a b c -> a b c

class FArrow a => FArrowChoice a where
  left :: a b c -> a (Either b d) (Either c d)
  right :: a b c -> a (Either d b) (Either d c)
  (|||) :: a b d -> a c d -> a (Either b c) d
  -- (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')

instance Monad m => FArrowChoice (FKleisli m) where
  left (FK f) = FK (MkFArr $ \x -> case x of
                                       Left b -> do c <- f' b
                                                    return $ Left c
                                       Right d -> return $ Right d)
    where f' = unArrow f
  right (FK f) = FK (MkFArr $ \x -> case x of
                                       Right d -> do c <- f' d
                                                     return $ Right c
                                       Left b -> return $ Left b)
    where f' = unArrow f

  (FK f) ||| (FK g) = FK (MkFArr $ \ x -> case x of
                                            Left d -> f' d
                                            Right d -> g' d)

    where f' = unArrow f
          g' = unArrow g

  --  (FK f) +++ (FK g) = FK (MkArr $ \x -> )

{-
is this a functor?
-}

-- instance Functor (a :->) where
  

