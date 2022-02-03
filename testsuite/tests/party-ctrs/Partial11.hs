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

module Partial11 where

import GHC.Types (type (@@))
import qualified GHC.Base as B(id,(.))
import Data.Type.Coercion
import Data.Type.Equality
import Data.Coerce (coerce)


import Partial10

infixr 9 .
infixr 1 >>>, <<<

infixr 5 <+>
infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||
infixr 1 ^>>, >>^
infixr 1 ^<<, <<^

  
type instance a @@ () = ()


class Category cat where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

instance Category (->) where
  id = B.id
  (.) = (B..)

{-# RULES
"identity/left" forall p .
                Partial11.id Partial11.. p = p
"identity/right"        forall p .
                p Partial11.. Partial11.id = p
-- "association"   forall p q r .
--                  (p Partial11.. q) Partial11.. r = p Partial11.. (q Partial11.. r)
 #-}


instance Category (:~:) where
  id          = Refl
  Refl . Refl = Refl

instance Category (:~~:) where
  id            = HRefl
  HRefl . HRefl = HRefl

instance Category Coercion where
  id = Coercion
  (.) Coercion = coerce

(<<<) :: Category cat => cat b c -> cat a b -> cat a c
(<<<) = (Partial11..)

(>>>) :: Category cat => cat a b -> cat b c -> cat a c
f >>> g = g Partial11.. f


class Category a => Arrow a where
  {-# MINIMAL arr, (first | (***)) #-}
  arr :: (b -> c) -> a b c

  first :: a b c -> a (b,d) (c,d)
  default first :: (a @@ d, a d @@ d, a (d, c) @@ (c, d), a @@ (d, c)
           , a (d, c) @@ (d, c), a @@ c, a c @@ c, a (c, d) @@ (d, c)
           , a (c, d) @@ (c, d), a @@ (c, d)) =>
    a b c -> a (b,d) (c,d)
  first = (*** Partial11.id)

  sectond :: a b c -> a (d,b) (d,c)
  default second :: (a @@ d, a d @@ d
            , a (b, d) @@ (d, c), a (c, d) @@ (d, c)
            , a @@ (c, d), a (b, d) @@ (c, d), a @@ (b, d), a (d, b) @@ (b, d)
            , a (d, b) @@ (d, b), a b @@ b
            , a (d, c) @@ (c, d), a @@ (d, c)
            , a (d, c) @@ (d, c), a @@ c, a c @@ c, a (c, d) @@ (c, d)
            , a (b, d) @@ (d, b), a (b, d) @@ (b, d))
         => a b c -> a (d,b) (d,c)
  second = (Partial11.id ***)

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


{-# RULES
"compose/arr"   forall f g .
                (arr f) Partial11.. (arr g) = arr (f Partial11.. g)
"first/arr"     forall f .
                first (arr f) = arr (first f)
"second/arr"    forall f .
                second (arr f) = arr (second f)
"product/arr"   forall f g .
                arr f *** arr g = arr (f *** g)
"fanout/arr"    forall f g .
                arr f &&& arr g = arr (f &&& g)
-- "compose/first" forall f g .
--                 (first f) Partial11.. (first g) = first (f Partial11.. g)
-- "compose/second" forall f g .
--                 (second f) Partial11.. (second g) = second (f Partial11.. g)
 #-}

instance Arrow (->) where
  arr f = f
  (***) f g ~(x,y) = (f x, g y)
  first = (*** Partial11.id)
  second = (Partial11.id ***)
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
    arr f = Kleisli (return' Partial11.. f)
    first (Kleisli f) = Kleisli (\ ~(b,d) -> bind' (f b) (\c -> return' (c,d)))
    second (Kleisli f) = Kleisli (\ ~(d,b) -> bind' (f b) (\c -> return' (d,c)))
    f *** g = first f >>> arr swap >>> first g >>> arr swap
      where swap ~(x,y) = (y,x)
    f &&& g = arr (\z -> (z,z)) >>> f *** g

returnA :: Arrow a => a b b
returnA = arr Partial11.id

-- | Precomposition with a pure function.
(^>>) :: (Arrow a, a b @@ c) => (b -> c) -> a c d -> a b d
f ^>> a = arr f >>> a

-- | Postcomposition with a pure function.
(>>^) :: (Arrow a, a c @@ d, a @@ c) => a b c -> (c -> d) -> a b d
a >>^ f = a >>> arr f

-- | Precomposition with a pure function (right-to-left variant).
(<<^) :: (Arrow a, a b @@ c) => a c d -> (b -> c) -> a b d
a <<^ f = a <<< arr f

-- | Postcomposition with a pure function (right-to-left variant).
(^<<) :: (Arrow a, a c @@ d, a @@ c) => (c -> d) -> a b c -> a b d
f ^<< a = arr f <<< a


class Arrow a => ArrowZero a where
    zeroArrow :: a b c

-- -- | @since 2.01
-- instance MonadPlus' m => ArrowZero (Kleisli m) where
--     zeroArrow = Kleisli (\_ -> mzero')

-- | A monoid on arrows.
class ArrowZero a => ArrowPlus a where
    -- | An associative operation with identity 'zeroArrow'.
    (<+>) :: a b c -> a b c -> a b c

-- instance MonadPlus' m => ArrowPlus (Kleisli m) where
--     Kleisli f <+> Kleisli g = Kleisli (\x -> f x `mplus'` g x)


class Arrow a => ArrowChoice a where
    -- {-# MINIMAL (left | (+++)) #-}

    left :: {- (a @@ d, a d @@ d
            , a (Either' c d) @@ Either' c d
            , a @@ Either' c d, a b @@ b
            , a (Either' d c) @@ Either' c d
            , a @@ Either' d c, a (Either' d c) @@ Either' d c, a @@ c, a c @@ c
            , a (Either' c d) @@ Either' d c
            , a (Either' d c) @@ Either' c d
            , a @@ Either' d c, a (Either' d c) @@ Either' d c, a @@ c, a c @@ c
            , a (Either' b d) @@ Either' d b, a (Either' b d) @@ Either' b d)
=> -} a b c -> a (Either' b d) (Either' c d)
    -- left = (+++ Partial11.id)

    right :: {- (a @@ d, a d @@ d, a (Either' d b) @@ Either' d b
             , a (Either' c d) @@ Either' d c
             , a @@ Either' c d, a (Either' b d) @@ Either' c d
             , a (Either' c d) @@ Either' c d, a (Either' b d) @@ Either' d c
             , a @@ Either' b d, a (Either' d b) @@ Either' b d, a b @@ b
             , a (Either' d c) @@ Either' c d
             , a @@ Either' d c, a (Either' d c) @@ Either' d c, a @@ c, a c @@ c
             , a (Either' b d) @@ Either' d b, a (Either' b d) @@ Either' b d)
          => -}
      a b c -> a (Either' d b) (Either' d c)
    -- right = (Partial11.id +++)

    (+++) :: {- (a (Either' b b') @@ Either' c b'
             , a @@ Either' c b', a (Either' c b') @@ Either' c c'
             , a b' @@ b', a (Either' c b') @@ Either' c b', a (Either' c b') @@ Either' b' c
             , a @@ Either' b' c, a (Either' b' c) @@ Either' c c'
             , a (Either' c' c) @@ Either' c' c, a c @@ c, a @@ c, a (Either' b' c) @@ Either' c' c
             , a @@ Either' c' c, a (Either' c' c) @@ Either' c c', a b @@ b
             , a (Either' b' c) @@ Either' b' c, a (Either' b' c) @@ Either' c b'
             , a c' @@ c', a @@ c', a (Either' c c') @@ Either' c c', a @@ Either' c c'
             , a (Either' c c') @@ Either' c' c, a (Either' b b') @@ Either' b b'
             , a (Either' b b') @@ Either' b' b)
          => -}
      a b c -> a b' c' -> a (Either' b b') (Either' c c')
    -- f +++ g = left f >>> arr mirror >>> left g >>> arr mirror
    --   where
    --     mirror :: Either' x y -> Either' y x
    --     mirror (Left' x) = Right' x
    --     mirror (Right' y) = Left' y

    (|||) :: {- (a (Either' b c) @@ Either' c b,
               a (Either' b c) @@ Either' b c, a (Either' d d) @@ Either' d d,
               a @@ d, a d @@ d, a (Either' c d) @@ Either' d c,
               a (Either' c d) @@ Either' c d, a b @@ b,
               a (Either' c d) @@ Either' d d, a @@ Either' c d,
               a (Either' d c) @@ Either' c d, a (Either' d c) @@ Either' d c,
               a c @@ c, a (Either' d c) @@ Either' d d, a @@ Either' d c,
               a (Either' b c) @@ Either' d c, a (Either' b c) @@ Either' d d,
               a @@ Either' d d, a (Either' d d) @@ d)
          => -}
      a b d -> a c d -> a (Either' b c) d
    -- f ||| g = f +++ g >>> arr untag
    --   where
    --     untag (Left' x) = x
    --     untag (Right' y) = y

-- {-# RULES
-- "left/arr"      forall f .
--                 left (arr f) = arr (left f)
-- "right/arr"     forall f .
--                 right (arr f) = arr (right f)
-- "sum/arr"       forall f g .
--                 arr f +++ arr g = arr (f +++ g)
-- "fanin/arr"     forall f g .
--                 arr f ||| arr g = arr (f ||| g)
-- -- "compose/left"  forall f g .
-- --                 left f Partial11.. left g = left (f Partial11.. g)
-- -- "compose/right" forall f g .
-- --                 right f Partial11.. right g = right (f Partial11.. g)
--  #-}

instance ArrowChoice (->) where
    left f = f +++ Partial11.id
    right f = Partial11.id +++ f
    f +++ g = (Left' Partial11.. f) ||| (Right' Partial11.. g)
    (|||) = either'


-- instance Monad' m => ArrowChoice (Kleisli m) where
--     left f = f +++ arr Partial11.id
--     right f = arr Partial11.id +++ f
--     f +++ g = (f >>> arr Left') ||| (g >>> arr Right')
--     Kleisli f ||| Kleisli g = Kleisli (either' f g)

class Arrow a => ArrowApply a where
    app :: a (a b c, b) c

instance ArrowApply (->) where
    app (f,x) = f x

-- newtype ArrowMonad a b = ArrowMonad (a () b)

-- instance Arrow a => Functor' (ArrowMonad a) where
--     fmap' f (ArrowMonad m) = ArrowMonad $ m >>> arr f

-- instance Arrow a => Applicative' (ArrowMonad a) where
--    pure' x = ArrowMonad (arr (const' x))
--    ArrowMonad f <<*>> ArrowMonad x = ArrowMonad (f &&& x >>> arr (uncurry Partial11.id))

-- instance ArrowApply a => Monad' (ArrowMonad a) where
--     bind' (ArrowMonad m) f = ArrowMonad $
--         m >>> arr (\x -> let ArrowMonad h = f x in (h, ())) >>> app


-- instance ArrowPlus a => Alternative' (ArrowMonad a) where
--    empty' = ArrowMonad zeroArrow
--    ArrowMonad x <<|>> ArrowMonad y = ArrowMonad (x <+> y)

-- instance (ArrowApply a, ArrowPlus a) => MonadPlus' (ArrowMonad a)


leftApp :: (ArrowApply a, a (Either' b d) @@ (a () (Either' c d), ()),
             a @@ (a () (Either' c d), ()),
             a (a () (Either' c d), ()) @@ Either' c d,
             a () @@ b, a b @@ Either' c d,
             a () @@ Either' c d, a @@ (), a @@ c, a c @@ Either' c d,
             a () @@ d, a @@ d, a d @@ Either' c d)
        => a b c -> a (Either' b d) (Either' c d)
leftApp f = arr ((\b -> (arr (\() -> b) >>> f >>> arr Left', ())) |||
             (\d -> (arr (\() -> d) >>> arr Right', ()))) >>> app

class Arrow a => ArrowLoop a where
    loop :: a (b,d) (c,d) -> a b c

-- | @since 2.01
instance ArrowLoop (->) where
    loop f b = let (c,d) = f (b,d) in c

