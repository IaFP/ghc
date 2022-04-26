
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module ImportAssoTypes where

import GHC.Types
import AssoTypes

blahType :: Elem a -> a
blahType = undefined


qqqq :: (ADF1 @ a, AssocDF1 a) => a -> a
qqqq = op1 . op2
