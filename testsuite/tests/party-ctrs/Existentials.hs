{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes, FlexibleInstances
             , TypeSynonymInstances
             , FlexibleContexts, ExistentialQuantification
             , ScopedTypeVariables, GeneralizedNewtypeDeriving
             , StandaloneDeriving
             , MultiParamTypeClasses
             , UndecidableInstances
             , ScopedTypeVariables, CPP, DeriveDataTypeable
             , PatternGuards
  #-}
{-# LANGUAGE  QuantifiedConstraints, DatatypeContexts
           , DefaultSignatures, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass, DataKinds, TypeApplications, UnliftedNewtypes, TypeFamilies, TypeOperators, PolyKinds #-}


module Existentials where


data Stack a = forall xs. Stack {
                 _self :: xs
               , _push :: (a -> xs -> xs)
               , _pop :: (xs -> xs)
               , _top :: (xs -> a)
               , _empty :: (xs -> Bool)
               }  


makeListStack :: [a] -> Stack a
makeListStack l = Stack l (:) tail head null

push :: forall a. a -> Stack a -> Stack a
push x (Stack self push' pop top empty) = Stack (push' x self) push' pop top empty

top :: forall a. Stack a -> a
top (Stack self push pop top' empty)
    = top' self

l1 = makeListStack [2, 3]
l2 = makeListStack [4, 5, 6]
-- testExpr :: [Int]
-- testExpr  = map (top `compose` push 1) (cons l1 (cons l2 []))
