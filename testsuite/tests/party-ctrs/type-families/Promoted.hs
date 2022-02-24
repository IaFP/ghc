{-# LANGUAGE DataKinds, GADTs, KindSignatures, PartialTypeConstructors #-}
module Problem10 where

{-------------------------------------------------------------------------------

CS:3820 Fall 2021 Problem of the Week #10
=========================================

This problem explores judgments and derivations, and how we can realize them as
datatypes in Haskell.  (There is a deeper connection between Haskell programs
and logical derivations, which we will discuss at the end of the semester.)

Formalizing the ≤ relation on ℕ 
================================

Our starting point is going to be derivation systems for the "less than or equal
to" relation on natural numbers.  We'll start by defining the natural numbers
(ℕ): ℕ consists of 0, along with the successor `S n` of every element `n ∈ ℕ`.
Or, to write it in Haskell:

-------------------------------------------------------------------------------}

data Nat = Z | S Nat

{-------------------------------------------------------------------------------

There's more than one way we could write a derivation system for the ≤ relation
on ℕ.  Here's one attempt:

    (Zero) ----------
           0 ≤ n

           m ≤ n
    (Succ) ---------
           S m ≤ S n

That is to say: we have two rules.  The first rule captures that 0 is less than
or equal to all other natural numbers.  The second captures that the successor
of m is less than the successor of n if m is less than n.  For example, if we
wanted to prove that 1 ≤ 4 (where I write 1 for the successor of 0, and 4 for
the successor of the successor of the successor of 1), we could give the
following derivation:

    (Zero) ------
           0 ≤ 3
    (Succ) ------
           1 ≤ 4

The next question is how we can represent these derivations in Haskell.  We
could try to define a normal data type to capture such derivations, like:

    data Lte = Zero | Succ Lte

The problem is that this data type captures nothing about how derivations relate
to the judgments they prove: the value `Succ Zero`, for example, is not
inherently any more a proof of `1 ≤ 4` or `1 ≤ 7` than it is `0 ≤ 2` or even `2
≤ 0`.

To connect these pieces, we can use a modern feature of Haskell called
"generalized algebraic data types".  The idea is that we can define a type `Lte
m n` that *only* contains values that prove that `m ≤ n`.  Our data type looks
something like this:

-------------------------------------------------------------------------------}

data Lte1 :: Nat -> Nat -> * where
    Zero :: Lte1 Z n
    Succ :: Lte1 m n -> Lte1 (S m) (S n)

{-------------------------------------------------------------------------------

The `Zero` constructor can *only* have types `Lte1 Z n` for any natural number
`n`.  So, for example, the following generates a type error:

>>> Zero :: Lte1 (S Z) (S Z)
Couldn't match type ‘'Z’ with ‘'S 'Z’
Expected type: Lte1 ('S 'Z) ('S 'Z)
  Actual type: Lte1 'Z ('S 'Z)

This doesn't mean that 1 isn't ≤ 1; it just means that the `Zero` rule doesn't
*prove* that `1 ≤ 1`.  (You can ignore all the single quotes in the error
message; those are essentially an implementation detail of the compiler.) Here's
another example:

>>> Succ Zero :: Lte1 (S (S Z)) (S (S (S (S Z))))
Couldn't match type ‘'Z’ with ‘'S 'Z’
Expected type: Lte1 ('S ('S 'Z)) ('S ('S ('S ('S 'Z))))
  Actual type: Lte1 ('S 'Z) ('S ('S ('S ('S 'Z))))

On the other hand, here's an example that works:

-------------------------------------------------------------------------------}

pr0_1 :: Lte1 (S (S Z)) (S (S (S (S Z))))
pr0_1 = Succ (Succ Zero)

{-------------------------------------------------------------------------------

You can tell this example works because it type checks!

Well, almost.  There is one way to produce something that seems to type, but
doesn't really correspond to the kind of proof we'd like:

-------------------------------------------------------------------------------}

pr0_2 :: Lte1 (S Z) Z
pr0_2 = pr0_2

{-------------------------------------------------------------------------------

This claims to be a proof that 1 ≤ 0, something we'd probably prefer not to be
able to prove.  Of course, there's no derivation in our derivation system that
would actually prove such a claim, but `pr0_2` gets around that by never
producing any result at all!  To rule this kind of thing out, we'll define a
function that only completes given a finite argument:

-------------------------------------------------------------------------------}

finite1 :: Lte1 m n -> ()
finite1 Zero        = ()
finite1 (Succ lte1) = finite1 lte1

{-------------------------------------------------------------------------------

Now we can say that a value `p` of the right type, and such that `finite p`
evaluates, is a proof.  We can even capture this using a silly `Show` instance:

-------------------------------------------------------------------------------}

instance Show (Lte1 m n) where
    show lte1 = case finite1 lte1 of () -> "proved!"

{-------------------------------------------------------------------------------

Now, we can see that `pr0_1` is a proof:

>>> pr0_1
proved!

But if we did the same thing for pr0_2, we'd never get the string "proved!",
because `finite1 pr0_2` will never actually return.

Problem 10-1
------------

Your first problem is to build terms (finite terms, that is) inhabiting the
following types; or, if you prefer, your first problem is to prove that:

* 0 ≤ 4
* 2 ≤ 2
* 2 ≤ 3

-------------------------------------------------------------------------------}

pr1_1 :: Lte1 Z (S (S (S (S Z))))
{- { asgn 
pr1_1 = undefined
-- } -}
-- { soln 
pr1_1 = Zero
-- } 

pr1_2 :: Lte1 (S (S Z)) (S (S Z))
{- { asgn 
pr1_2 = undefined
-- } -}
-- { soln
pr1_2 = Succ (Succ Zero)
-- }

pr1_3 :: Lte1 (S (S Z)) (S (S (S Z)))
{- { asgn
pr1_3 = undefined
-- } -}
-- { soln
pr1_3 = Succ (Succ Zero)
-- }

{-------------------------------------------------------------------------------

Formalizing the reflexive transitive closure, version 1
=======================================================

Another way we can think of formalizing ≤ is as the reflexive, transitive
closure of a simpler relation, which I'll call <₁.   The simpler relation
captures exactly one step of a less than proof:

    (Lt1) --------
          n <₁ S n

That is to say: each number is 1 less than it's immediate successor.  The idea
of the reflexive transitive closure of a relation is that it captures
*sequences* of that relation, of length ≥ 0.  Let's (just for the moment) write
the reflexive transitive closure of <₁ as <₁*.  Then we would have:

    4 <₁* 4

(because that's 0 instances of <₁)

    4 <₁* 5

(because that's 1 instance of <₁)

    4 <₁* 8

(because that's 3 instances of <₁)

and so forth.  As you won't be surprised to notice, the reflexive transitive
closure of <₁ is exactly the ≤ relation.  We can formalize this in Haskell as
follows (where our representation of `n many instances` is remarkably similar to
how we might think of lists...)

-------------------------------------------------------------------------------}

data Lt1 :: Nat -> Nat -> * where
    Lt1 :: Lt1 m (S m)

fin2_1 :: Lt1 m n -> ()
fin2_1 Lt1 = ()

data Lte2 :: Nat -> Nat -> * where
    Done :: Lte2 m m
    Step :: Lt1 m n -> Lte2 n p -> Lte2 m p

fin2_2 :: Lte2 m n -> ()
fin2_2 Done = ()
fin2_2 (Step lt1 lte2) = fin2_1 lt1 `seq` fin2_2 lte2

instance Show (Lte2 m n) where
    show lte2 = fin2_2 lte2 `seq` "proved!"

{-------------------------------------------------------------------------------

We can still show that 2 ≤ 3:

>>> Step Lt1 Done :: Lte2 (S (S Z)) (S (S (S Z)))
proved!

But we can't just use any old proof for any intuitively true judgment:

>>> Done :: Lte2 (S (S Z)) (S (S (S Z)))
Couldn't match type ‘'Z’ with ‘'S 'Z’
Expected type: Lte2 ('S ('S 'Z)) ('S ('S ('S 'Z)))
  Actual type: Lte2 ('S ('S 'Z)) ('S ('S 'Z))

Problem 10-2
------------

Your next problem is to repeat the previous problem, but with this new proof
theory.  That is to say, you should find terms inhabiting the following types,
proving that:

* 0 ≤ 4
* 2 ≤ 2
* 2 ≤ 3

-------------------------------------------------------------------------------}

pr2_1 :: Lte2 Z (S (S (S (S Z))))
{- { asgn
pr2_1 = undefined
-- } -}
-- { soln 
pr2_1 = Step Lt1 (Step Lt1 (Step Lt1 (Step Lt1 Done)))
-- }

pr2_2 :: Lte2 (S (S Z)) (S (S Z))
{- { asgn
pr2_2 = undefined
-- } -}
-- { soln
pr2_2 = Done
-- }

pr2_3 :: Lte2 (S (S Z)) (S (S (S Z)))
{- { asgn
pr2_3 = undefined
-- } -}
-- { soln
pr2_3 = Step Lt1 Done
-- } 

{-------------------------------------------------------------------------------

Formalizing the reflexive transitive closure, version 2
=======================================================

You may have noticed that our formalization of the "reflexive transitive
closure" of <₁ seemed to make no reference to reflexivity or transitivity.
Another way we could formalize the reflexive transitive closure is to add
reflexive and transitive cases directly; we might end up with the following
rules:


    (Refl)   ------
             n ≤ n
  
             m <₁ n
    (Inst)   -------
             m ≤ n

    (Trans)  m ≤ n       n ≤ p
             -----------------
             m ≤ p

The (Inst) rule might seem silly---why not just have `m ≤ S m`, but the point is
that this is a *general* construction, applicable to relations beyond just <₁.

We can capture this construction in Haskell as follows.

-------------------------------------------------------------------------------}

data Lte3 :: Nat -> Nat -> * where
    Refl  :: Lte3 m m
    Inst  :: Lt1 m n -> Lte3 m n
    Trans :: Lte3 m n -> Lte3 n p -> Lte3 m p

fin3 :: Lte3 m n -> ()
fin3 Refl = ()
fin3 (Inst lt1) = fin2_1 lt1
fin3 (Trans lte3 lte3') = fin3 lte3 `seq` fin3 lte3'

instance Show (Lte3 m n) where
    show lte3 = fin3 lte3 `seq` "proved!"

{-------------------------------------------------------------------------------

And again, we can prove that 2 ≤ 3, for example:

>>> Trans (Inst Lt1) Refl :: Lte3 (S (S Z)) (S (S (S Z)))
proved!

but not

>>> Trans Refl Refl :: Lte3 (S (S Z)) (S (S (S Z)))
Couldn't match type ‘'Z’ with ‘'S 'Z’
Expected type: Lte3 ('S ('S 'Z)) ('S ('S ('S 'Z)))
  Actual type: Lte3 ('S ('S 'Z)) ('S ('S 'Z))

Problem 10-3
------------

Your final problem is, once again, to prove that

* 0 ≤ 4
* 2 ≤ 2
* 2 ≤ 3

with our final formalization of ≤, by giving terms that inhabit the types below.
However, there is a twist.  This formalization can provide multiple different
proofs of the same judgment.  You should demonstrate this by providing two
*different* proofs that 0 ≤ 4 (as `pr3_1a` and `pr3_1b`)

-------------------------------------------------------------------------------}    

pr3_1a :: Lte3 Z (S (S (S (S Z))))
{- { asgn 
pr3_1a = undefined
-- } -}
-- { soln
pr3_1a = Trans (Inst Lt1) (Trans (Inst Lt1) (Trans (Inst Lt1) (Inst Lt1)))
-- }

pr3_1b :: Lte3 Z (S (S (S (S Z))))
{- { asgn 
pr3_1b = undefined
-- } -}
-- { soln
pr3_1b = Trans (Trans (Inst Lt1) (Inst Lt1)) (Trans (Inst Lt1) (Inst Lt1))
-- }

pr3_2 :: Lte3 (S (S Z)) (S (S Z))
{- { asgn 
pr3_2 = undefined
-- } -}
-- { soln
pr3_2 = Refl
-- }

pr3_3 :: Lte3 (S (S Z)) (S (S (S Z)))
{- { asgn 
pr3_3 = undefined
-- } -}
-- { soln 
pr3_3 = Inst Lt1
-- }



