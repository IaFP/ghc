{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module T13272 where

import GHC.Generics
import GHC.Types (Total)

class TypeName a where
  typeName         :: forall proxy.
                      proxy a -> String
  default typeName :: forall proxy d f.
                      (Generic a, Rep a ~ D1 d f, Datatype d, Total f)
                   => proxy a -> String
  typeName _ = gtypeName $ from (undefined :: a)

gtypeName :: Datatype d => D1 d f p -> String
gtypeName = datatypeName

data T a = MkT a
  deriving (Generic, TypeName)
