{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module InferredArity where


-- From F we generate WF_F,
-- which copies bindings from F.
-- We want to avoid a situation where
-- WF_F and F's binders get out of sync.
-- By default, a has kind *.
type family F a

-- Choose your favorite HO operator
data Fix f = In (f (Fix f))

-- a's arity has magically changed from * to (* -> *)
type instance F f = Fix f
