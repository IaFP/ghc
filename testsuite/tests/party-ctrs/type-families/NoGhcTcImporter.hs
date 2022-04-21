{-# LANGUAGE AllowAmbiguousTypes     #-}      -- for pprIfTc, etc.
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveDataTypeable      #-}
{-# LANGUAGE EmptyDataDeriving       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableSuperClasses #-} -- for IsPass; see Note [NoGhcTc]
{-# LANGUAGE UndecidableInstances    #-} -- Wrinkle in Note [Trees That Grow]

module NoGhcTcImporter where

import NoGhcTc

g'' :: GhcPass (NoGhcTcPass 'Renamed)
g'' = GhcRn


bug :: GhcPass (NoGhcTcPass 'Renamed)
bug = g''
