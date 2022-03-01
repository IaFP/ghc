{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module AssociatedImporter where

import Associated


-- Should have constraint WF_Elem a => ...
foobar' :: Elem a -> a
foobar' = undefined
