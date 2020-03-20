> {-# LANGUAGE RankNTypes #-}
> module Notes where

> import GHC.Types (type (@@))
** Notes on A Hierarchy of Mendler style Recursion Combinators **

... concerning a divergent histomorphism over a negative datatype.

--------------------------------------------------------------------------------
Problem statement
--------------------------------------------------------------------------------

Mendler catamorphisms and paramorphisms provably terminate over datatypes of any
polarity. Histomorphisms terminate for positive datatypes, but not negative datatypes.

In the paper, Ahn and Sheard give an example (which I reproduce below) of a
non-terminating histomorphism over a negative datatype. They then pose the
following open question: Is there a safe way to apply strong induction (i.e,
histomorphisms) to negative inductive datatypes?

--------------------------------------------------------------------------------

Ahn et al give the following example:

> data T = C (T -> ())
> p :: T -> (T -> ()) -- destructor
> p (C f) = f
>
> w :: T -> ()
> w x = (p x) x

now consider the term w (C w) :: T.
  w (C w)
→ (p (C w)) (C w)
→ w (C W)
→ ...

This is a divergent term without any explicit recursion. Mendler catamorphism
 prevents this divergence (see section 3.8), but a mendler histomorphism does
 not. To demonstrate:

First define the typical morphisms and Mu.

> newtype Functor f => Mu f = In { out :: (f (Mu f)) }
> 
> newtype  Functor f => McataAlg f x = McataAlg (forall r . (f @@ r) => (r -> x) -> f r -> x) 
> mcata :: McataAlg f x -> Mu f -> x
> mcata (McataAlg alg) (In d) = alg (mcata alg) d
>
> newtype Functor f =>  MhistoAlg f x = MhistoAlg (forall r. (f @@ r) => (r -> f r) -> (r -> x) -> f r -> x)
> mhisto :: MhistoAlg f x -> Mu f -> x
> mhisto (MhistoAlg alg) (In d) = alg out (mhisto alg) d

Now define the example (negative) datatype.

> data FooF r = Noo | Coo (r -> r) r
> type Foo = Mu FooF
>
> noo = In Noo
> coo f xs = In (Coo f xs)

Demonstration of a terminating catamorphism, getting the length of a Foo
 datatype.

> lenFoo :: Foo -> Int
> lenFoo = mcata alg where
>   alg _ Noo = 0
>   alg len (Coo f xs) = 1 + len (f xs)

-- >>> lenFoo noo
-- 0
-- >>> lenFoo (coo id (coo id noo))
-- 2

But we can use mhisto to loop.

> loopFoo :: Foo -> Int
> loopFoo = mhisto alg where
>   alg :: MhistoAlg FooF Int
>   alg _ _ Noo = 0
>   alg out' eval (Coo f xs) = case (out' xs) of
>                               Noo        -> 1 + eval (f xs)
>                               (Coo f' _) -> 1 + eval (f' xs)


> foo :: Foo
> coo0, coo1 :: Foo -> Foo
> coo0 = coo id
> coo1 = coo coo0
> foo = coo0 (coo1 noo)

-- >>> loopFoo foo
-- *** Exception: stack overflow

--------------------------------------------------------------------------------
- The way out of "out"
--------------------------------------------------------------------------------

We can provide a definition of mendler histomorphism that doesn't diverge if we
rid ourselves of "out" and replace it with case'. The idea here is to introduce
another type variable that can be cast back up to r. Then in place of out, have
instead:

case  :: r -> forall x. (forall r'. (r' -> r) -> f r' -> x) -> x

this means, with respect to the loopFoo example, you would have

alg :: MhistoAlg FooF Int
alg _ _ Noo = 0
alg case' eval (Coo f xs) = case' xs (\ _ ys ->  case ys of
    Noo          -> 1 + eval (f xs)
    (Coo f' zs)  -> 1 + eval (f' xs) -- **NO LONGER**  type checks because xs :: r
                                     -- and f' :: r' -> r'.
)

And so, as noted, you can no longer recurse on xs via (f' xs). For reference:

  xs :: r
  f  :: r -> r

  ys :: f r'
  f' :: r' -> r'
  zs :: r'



