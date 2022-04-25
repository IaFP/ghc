{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module AssoTypes where

class Collection a where
  type Elem a
  e :: a
  cons :: Elem a -> a -> a


instance Collection [a] where
  type Elem [a] = a
  e = []
  cons = (:)


class AssocDF1 a where
  data ADF1 a
  op1 :: ADF1 a -> a 


class Assoc2DF a where
  data ADF2 a
  type ATF2 a 
  op2 :: a -> (ADF2 a, ATF2 a)
  -- should elaborate to
  -- op2 :: (ADF @ a, $wf:ATF2 a) => a -> (ADF a, ATF2 a)
  
