{-# LANGUAGE RankNTypes #-}

module T14488 where
import GHC.Types (type (@@))
type Lens' s a = forall f. (Functor f, f @@ a, f @@ s) => (a -> f a) -> s -> f s

data T a = MkT { _tfield :: Eq a => a }

tfield :: Eq a => Lens' (T a) a
tfield f t = MkT <$> f (_tfield t)
