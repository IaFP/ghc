{-# LANGUAGE PolyKinds, StandaloneDeriving, GeneralizedNewtypeDeriving
           , RoleAnnotations, RankNTypes, ExistentialQuantification, TypeFamilies, TypeFamilyDependencies #-}

module NewType where
import GHC.Types (Total, WFT)

-- newtype NT f a = MkNT {unNT :: f a}

data B a = B a

class C a where
  type family T a = r | r -> a
  mblah :: T a -> B a
  tblah :: B a -> T a

instance C Bool where
  type instance T Bool = Bool
  mblah b = B b
  tblah (B b) = b
  
newtype ECP = ECP {unECP :: forall a. (WFT (T a), C a) => B a}

f :: ECP -> ECP 
f ecp = ECP $ mblah (tblah (unECP ecp))


data Ord a => Tree a = Tip | Branch a (Tree a) (Tree a)
