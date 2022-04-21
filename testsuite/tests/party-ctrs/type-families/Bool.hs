{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy              #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}


module Bool where
  
import GHC.Base
-- import Data.Type.Bool hiding ((&&))

type family a && b where
  'False && a      = 'False
  'True  && a      = a
  a      && 'False = 'False
  a      && 'True  = a
  a      && a      = a
infixr 3 &&
