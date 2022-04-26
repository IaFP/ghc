{-# LANGUAGE PolyKinds, StandaloneDeriving, GeneralizedNewtypeDeriving
           , RoleAnnotations, RankNTypes, ExistentialQuantification, TypeFamilies, TypeFamilyDependencies #-}

module NewType where

import GHC.Types (type (@))
-- simple examples 
-- data Ord a => T a = T | B a (T a) (T a)
-- newtype Eq a => NT a = NT {unNT :: T a} -- not okay as Eq a |/- Ord a

data Eq a => Key a = Key a
newtype Ord a => Map a b = Map {unMap :: Key a -> b} -- okay as Ord a |- Eq a

newtype f @ a => Rec f a = Rec {unRec :: f a} -- Okay as f @ a |- f @ a
                                              -- but does the solver know about this? -- Yes

newtype Ord a => RecNT a = RecNT {unRecNT :: RecNT [a] -> a } -- this is okay
