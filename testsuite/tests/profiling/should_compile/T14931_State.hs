{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitNamespaces, TypeOperators #-}
module T14931_State (MonadState(..), Lazy.evalState) where
import GHC.Types (Total, type (@@))

import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT, get, put, evalState)

class Monad m => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()

instance (Total m, m @@ ((), s), m @@ (s, s), Monad m) => MonadState s (Lazy.StateT s m) where
    get = Lazy.get
    put = Lazy.put
