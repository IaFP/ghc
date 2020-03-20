{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -ddump-simpl-stats #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, ExplicitNamespaces #-}

-- The rule foo/bar should fire

module Roman where
import GHC.Types (Total, type (@@))

foo :: (m @@ a, m @@ b) => (forall m0. (m0 @@ a, m0 @@ b) => m0 a -> m0 b) -> m a -> m b
{-# NOINLINE foo #-}
foo f = f

bar :: (forall m0. m0 @@ a => m0 a -> m0 a) -> m a -> m a
bar f = f

-- {-# RULES "foo/bar" foo = bar #-}

blip = bar id

