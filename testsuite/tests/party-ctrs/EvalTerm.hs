{-# LANGUAGE RankNTypes, FlexibleInstances
             , TypeSynonymInstances
             , FlexibleContexts, ExistentialQuantification
             , ScopedTypeVariables, GeneralizedNewtypeDeriving
             , StandaloneDeriving
             , MultiParamTypeClasses
             , UndecidableInstances
             , ScopedTypeVariables, CPP, DeriveDataTypeable
             , PatternGuards
  #-}
{-# LANGUAGE PartialTypeConstructors, QuantifiedConstraints, DatatypeContexts
           , DefaultSignatures, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass, DataKinds, TypeApplications, UnliftedNewtypes, TypeFamilies, TypeOperators, PolyKinds #-}

module EvalTerm where

import GHC.Types (type (@), Total)
import qualified Control.Monad.Trans.Writer as Writer
import Control.Monad.Trans.Reader
import qualified Data.IntMap as Map
-- import Control.Monad
import Control.Monad.Catch
-- import Control.Monad.Trans
-- import Control.Monad.State
-- import Control.Monad.Reader
-- import System.Console.Haskeline.Monads

class Term (n:: * -> *)

class CommandMonad (n :: k -> k')
data Grapheme = Grapheme { gBaseChar :: Char
                         , combiningChars :: [Char]}
                    deriving Eq

type Prefix = [Grapheme]

type LineChars = ([Grapheme],[Grapheme])

data Effect = LineChange (Prefix -> LineChars)
              | PrintLines [String]
              | ClearScreen
              | RingBell

data CmdM m a   = GetKey (KeyMap (CmdM m a))
                | DoEffect Effect (CmdM m a)
                | CmdM (m (CmdM m a))
                | Result a

data Key = Key String Int

data KeyMap a = KeyMap {lookupKM :: Key -> Maybe (KeyConsumed a)}

data KeyConsumed a = NotConsumed a | Consumed a

-- type KeyCommand m s t = KeyMap (Command m s t)

data EvalTerm m = forall n. (Total m, Total n, Term n, CommandMonad n) => EvalTerm (forall a. n a -> m a) (forall a. m a -> n a)
