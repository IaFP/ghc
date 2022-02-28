{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, UndecidableInstances, StandaloneDeriving, TypeFamilies #-}

module ClassElab where
-- import GHC.Types (type (@))

-- class Functr f where
--   fmap' :: (a -> b) -> f a -> f b

-- class C a where
--   i :: a -> a

-- data m @ a => ReaderT r m a = ReaderT (r -> m a)

-- class TransMonad t where
--   blah :: m a -> t m a 

data D

type family XRec a b


class C (XRec a b) where
  unXRec :: XRec a b -> XRec a b


instance UnXRec a D where
  unXRec = undefined
