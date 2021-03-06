-- Check that pattern match failure in do-notation
-- is reflected by calling the monadic 'fail', not by a
-- runtime exception
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators, TypeFamilies #-}

import Control.Monad
import Control.Monad.Fail
import Data.Maybe
import GHC.Types (type (@))
test :: (MonadPlus m, MonadFail m, m @ [a]) => [a] -> m Bool
test xs
  =   do
        (_:_) <- return xs
                -- Should fail here
        return True
    `mplus`
        -- Failure in LH arg should trigger RH arg
      do
        return False

main :: IO ()
main
  = do  let x = fromJust (test [])
        putStrLn (show x)
