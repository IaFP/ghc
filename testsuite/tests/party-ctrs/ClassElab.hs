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

class C a

-- class UnXRec a where
--   unXRec :: XRec a b -> XRec a b


-- instance UnXRec a D where
--   unXRec = undefined
type family F t
type Wft t = t ~ t

blah :: forall t. (Wft (F t), C t) => t -> t
blah = id

data Identity a = I a

optionalFieldDef
    :: (Functor (g s), c (Identity a), Eq a)
    => D  -- ^ field name
    -- -> ALens' s a  -- ^ @'Lens'' s a@: lens into the field
    -> a           -- ^ default value
    -> g s a
optionalFieldDef = undefined
