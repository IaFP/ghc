{-# LANGUAGE Unsafe, DatatypeContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators, TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE PartialTypeConstructors, GADTs #-}


{- Taken from: https://www.schoolofhaskell.com/user/bartosz/understanding-algebras -}

module FAlgebra where

-- F-Algebras 

newtype Functor f => FAlgebra f a = FAlg (f a -> a)
newtype Functor f => Fix f = Fix { unFix :: f (Fix f) }
                          -- unFix :: Functor f => Fix f -> f (Fix f)

type InitAlg f = FAlgebra f (Fix f)

initAlg :: InitAlg f
initAlg = FAlg $ Fix

cata :: FAlgebra f a -> Fix f -> a
cata (FAlg alg) = alg . fmap (cata (FAlg alg)) . unFix

--- F-Coalgebras

newtype Functor f => FCoAlgebra f a = FCoAlg (a -> f a)
newtype Functor f => Final f = Final { unFinal :: f (Final f) }

type FinalAlg f = FCoAlgebra f (Final f)

finalCoAlg :: FinalAlg f
finalCoAlg = FCoAlg unFinal


-- Algera of simple Arithmetic Expressions

data AExprF a = Const Int | Add a a | Mult a a


type AExpr = Fix AExprF

e1 :: AExpr
e1 = Fix $ Const 1

e2 :: AExpr
e2 = Fix ((Fix ((Fix $ Const 1) `Add` (Fix $ Const 2))) `Mult` (Fix $ Const 5))


instance Functor AExprF where
  fmap _ (Const i) = Const i
  fmap eval (Add e1 e2) = (eval e1) `Add` (eval e2)
  fmap eval (Mult e1 e2) = (eval e1) `Mult` (eval e2)
  

type IntegerAlg = FAlgebra AExprF Int

iAlg :: IntegerAlg
iAlg = FAlg (\e -> case e of
                     Const i ->  i
                     Add x y ->  x + y
                     Mult x y ->  x * y)

sAlg :: FAlgebra AExprF String
sAlg = FAlg (\e -> case e of
                     Const i -> show i
                     Add x y -> " (" ++ x ++ " + " ++ y ++ ") "
                     Mult x y -> " (" ++ x ++ " * " ++ y ++ ") "                     
             )

evalAExpr :: AExpr -> Int
evalAExpr = cata iAlg

showAExpr :: AExpr -> String
showAExpr = cata sAlg 

v1 :: Int
v1 = evalAExpr e2

v2 :: String
v2 = showAExpr e2

--- Algebra of Lists i.e. Folding

data ListF a b = Nil | Cons a b

type IntListF = ListF Int

type List a = Fix (ListF a)

instance Functor (ListF a) where
    fmap _ Nil = Nil
    fmap f (Cons e x) = Cons e (f x)

sumListAlg :: FAlgebra IntListF Int
sumListAlg = FAlg (\e -> case e of
                        Nil -> 0
                        Cons e acc -> e + acc
              )
               
showListAlg :: Show e => FAlgebra (ListF e) String
showListAlg = FAlg (\e -> case e of
                        Nil -> "[]"
                        Cons e acc -> (show e) ++ " :: " ++ acc
              )

type IntList = List Int

lst_ints :: IntList
lst_ints = Fix $ Cons 2 (Fix $ Cons 3 (Fix $ Cons 4 (Fix Nil)))

lst_char :: List Char
lst_char = Fix $ Cons 'a' (Fix $ Cons 'b' (Fix $ Cons 'c' (Fix Nil)))


sumList :: IntList -> Int
sumList = cata sumListAlg

showList :: Show s => Fix (ListF s) -> String
showList = cata showListAlg

v3 = sumList lst_ints

-- CoAlgebra of Lists i.e unFolding

final_list :: Final (ListF a)
final_list = Final Nil


-- Streams and CoAlgebras
data StreamF a x = S a x

instance Functor (StreamF a) where
  fmap f (S a x) = S a (f x)

endsIn0s :: FCoAlgebra (StreamF Int) [Int]
endsIn0s = FCoAlg (\f -> case f of
                           []     -> S 0 []
                           (x:xs) -> S x xs)

nats :: Final (StreamF Int)
nats = g 0 where g n = Final (S n (g $ n+1))


--  Natural Numbers
data NatF x = Zero | Succ x

instance Functor NatF where
  fmap _ Zero = Zero
  fmap f (Succ x) = Succ (f x)

omega :: Final NatF
omega = f 0
  where f x = Final (Succ (f x))

n :: Int -> Final NatF
n x = f x
  where f 0 = Final Zero
        f n = Final (Succ (f $ n-1))
  
data Value = VInt Int | VBool Bool deriving Show

-- How about them Lambdas

-- data LExprF :: (* -> *) -> * -> * where
--   Var :: String -> LExpr a
--   Lam :: String -> LExpr a -> LExpr a
--   App :: LExpr a -> LExpr a -> LExpr a


-- type LExpr = Fix LExprF


