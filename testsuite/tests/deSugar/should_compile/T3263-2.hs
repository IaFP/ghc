-- #3263.  New kind of warning on monadic bindings that discard a monadic result

module T3263 where

import Control.Monad.Fix
import GHC.Types (type (@))
-- No warning
t1 :: Monad m => m Int
t1 = do
  return 10

-- No warning
t2 :: Monad m => m (m Int)
t2 = return (return 10)

-- No warning
t3 :: (Monad m, m @ Integer) => m (m Int) --Huh?
t3 = do
  return 10
  return (return 10)

-- Warning
t4 :: forall m. (Monad m, m @ m Int) => m Int
t4 = do
  return (return 10 :: m Int)
  return 10

-- No warning
t5 :: forall m. (Monad m, m @ m Int) => m Int
t5 = do
  _ <- return (return 10 :: m Int)
  return 10

-- Warning
t6 :: forall m. (MonadFix m, m @ m Int) => m Int
t6 = mdo
  return (return 10 :: m Int)
  return 10
