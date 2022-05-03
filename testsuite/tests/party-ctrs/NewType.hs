{-# LANGUAGE PolyKinds, StandaloneDeriving, GeneralizedNewtypeDeriving
           , RoleAnnotations, RankNTypes, ExistentialQuantification, TypeFamilies, TypeFamilyDependencies #-}

module NewType where

import GHC.Types (type (@), WFT)
-- simple examples 
data Ord a => T a = T | B a (T a) (T a)
data M a b = M a b
type family MaybeReduce a

data Eq a => Key a = Key a
newtype Ord a => Map a b = Map {unMap :: Key a -> b} -- okay as Ord a |- Eq a

newtype f @ a => Rec f a = Rec {unRec :: f a} -- Okay as f @ a |- f @ a
                                              -- but does the solver know about this? -- Yes

newtype Ord a => RecNT a = RecNT {unRecNT :: RecNT [a] -> a } -- this is okay

newtype TreeMap a b = TreeMap (M a (Maybe b, TreeMap a b)) -- also okay as M is Total and so is TreeMap

newtype Ord a => OTreeMap a b = OTreeMap (M [a] (Maybe b, OTreeMap a b)) -- mmm this? .. also okay

newtype WFT (MaybeReduce a) => NTWithTyFam a = NTWithTyFam [MaybeReduce a] -- elaborate WFT and we are okay

newtype Ord a => NTRec1 a = NTRec1 {unntRec1 :: NTRec2 a}
newtype Ord a => NTRec2 a = NTRec2 {unntRec2 :: NTRec1 a} -- This Works fine too
-- newtype Eq a => NTRec2 a = NTRec2 {unntRec2 :: NTRec1 a} -- Not okay 
-- newtype Eq a => NT a = NT {unNT :: T a} -- not okay as Eq a |/- Ord a
-- newtype NT a = NT {unNT :: T a} -- not okay as () |/- Ord a


newtype m @ b => Kleisli m a b = Kleisli { runKleisli :: a -> m b }
                                deriving Functor -- This is new
