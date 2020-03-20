{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE PartialTypeConstructors #-}
{-# LANGUAGE RankNTypes, GADTs #-}
{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}

-- Some examples from constrained monads
module ConstrainedMonads where
-- import GHC.Types (type (@@))
import qualified Data.Set as S
import GHC.Base (Applicative (..))
import Data.Complex
import Control.Monad.State

-------------------------------
------     Set Example  -------
-------------------------------
data Ord a => Set a = Set (S.Set a)
                    deriving (Show, Ord, Eq)    


map'set :: (a -> b) -> Set a -> Set b
map'set f (Set s) = Set (S.map f s)

instance Functor Set where
  fmap f s = map'set f s
  
instance Applicative Set where
  pure a = singleton a
  (<*>) _ _ = undefined
  liftA2 _ _ _ = undefined


instance Monad Set where
  m >>= f = flatten $ fmap f m
    

-- All these operations need not have the Ord a constraints anymore
empty :: Set a
empty = Set (S.empty)

singleton :: a -> Set a
singleton = Set . S.singleton

toList :: Set a -> [a]
toList (Set s) = S.toList s

fromList :: [a] -> Set a
fromList l = Set $ S.fromList l

union :: Set a -> Set a -> Set a
union (Set s1) (Set s2) = Set $ S.union s1 s2

insert :: a -> Set a -> Set a
insert a (Set s) = Set (S.insert a s)

delete :: a -> Set a -> Set a
delete a (Set s) = Set (S.delete a s)

flatten :: Set (Set a) -> Set a
flatten a = foldr union empty (toList a)

member :: a -> Set a -> Bool
member a (Set s) = S.member a s

notMember :: a -> Set a -> Bool
notMember a = not . (member a)



-------------------------------
------   Vector Example  -------
-------------------------------


class Finite a where
  enumerate :: [a]

newtype (Eq a, Finite a) => Vec a = Vec (a -> Complex Double)

returnVec :: a -> Vec a
returnVec a = Vec $ \b -> if a == b then 1 else 0

bindVec :: Vec a -> (a -> Vec b) -> Vec b
bindVec (Vec va) k = Vec $ \b -> sum [va a * ((k a) `apVec` b) | a <- enumerate]

apVec :: Vec a -> a -> Complex Double
apVec (Vec a) b = a b


instance Functor Vec where
  fmap _ _ = undefined

instance Applicative Vec where
  pure   = returnVec
  liftA2 _ _ _= undefined
  (<*>) _ _ = undefined

instance Monad Vec where
  (>>=) = bindVec
  
---------------------------------
------   Sunroof Example  -------
---------------------------------


type JSString = String
type JSBool = Bool

class Show a => Sunroof a where
  mkVar :: String -> a -- New variable of type a
  showJS :: a -> JSCode -- Show as JavaScript
  assignVar :: a -> JSCode -> JSCode -- Assign to a variable
  assignVar a c = showJS a ++ "=" ++ c ++ ";"

instance Sunroof () where
  mkVar _ = ()
  showJS () = "null"
  assignVar () _ = ""

instance Sunroof JSString where
  mkVar s = s
  showJS s = s

instance Sunroof JSBool where
  mkVar b = if b == "True" then True else False
  showJS s = show s

data Sunroof a => JS a where
  Prompt :: JSString -> JS JSString
  Alert :: JSString -> JS ()
  If :: Sunroof a => JSBool -> JS a -> JS a -> JS a

  Return :: a -> JS a
  Bind :: JS x -> (x -> JS a) -> JS a

type JSCode = String

type CompM a = State Int a

newVar :: CompM a
newVar = undefined

compileJS :: Sunroof a => JS a -> CompM (JSCode, a)
compileJS (Prompt s) = do
  (decl, v) <- newVar
  return (concat [decl
                 , assignVar v ("prompt(" ++ showJS s ++ ")")], v)
compileJS (Alert s) =
  return (concat ["alert(", showJS s, ");"], ())
compileJS (If b ja1 ja2) = do
  (decl, v) <- newVar
  (c1, a1) <- compileJS ja1
  (c2, a2) <- compileJS ja2
  return (concat [decl
                 , "if(", showJS b, ") {"
                 , c1, assignVar v (showJS a1)
                 , "} else {"
                 , c2, assignVar v (showJS a2)
                 , "}"], v)

instance Functor JS where
  fmap _ _ = undefined

instance Applicative JS where
  pure         = Return
  liftA2 _ _ _ = undefined
  (<*>) _ _    = undefined
  
instance Monad JS where
  (>>=) = Bind
