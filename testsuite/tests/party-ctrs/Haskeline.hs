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

module Haskeline where

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

data Event

type Command m s t = s -> CmdM m t

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

type KeyCommand m s t = KeyMap (Command m s t)

data EvalTerm m = forall n. (Total n, Term n, CommandMonad n) => EvalTerm (forall a. n a -> m a) (forall a. m a -> n a)

data TermOps = TermOps
  { withGetEvent :: forall m a . (CommandMonad m) => (m Event -> m a) -> m a
  , evalTerm :: forall m . (CommandMonad m) => EvalTerm m
  , saveUnusedKeys :: [Key] -> IO ()
  , externalPrint :: String -> IO ()
  }


runCommandLoop :: (CommandMonad m, Total m)
               => TermOps -> String -> KeyCommand m s a -> s -> m a
runCommandLoop tops@TermOps{evalTerm = e} prefix cmds initState
    = case e of
        EvalTerm eval liftE
            -> eval $ withGetEvent tops
                $ runCommandLoop' liftE tops (stringToGraphemes prefix) initState cmds 


runCommandLoop' :: forall m n s a . (Term n, CommandMonad n
                                    , Total m, Total n)
                => (forall b. m b -> n b) -> TermOps -> Prefix -> s -> KeyCommand m s a -> n Event -> n a
runCommandLoop' liftE tops prefix initState cmds getEvent = undefined

stringToGraphemes :: String -> [Grapheme]
stringToGraphemes s = [Grapheme {gBaseChar = 'g', combiningChars = s}]


-- newtype DumbTerm m a = DumbTerm {unDumbTerm :: StateT Window (PosixT m) a}
--                 deriving (Functor
--                          -- , Applicative
--                          -- , Monad
--                          , MonadIO,
--                           MonadThrow, MonadCatch, MonadMask,
--                           MonadState Window, MonadReader Handles)

-- instance Total (DumbTerm m)

-- instance (Total m, Monad m) => Applicative (DumbTerm m) where
--    pure a = DumbTerm (SS.StateT $ \s -> return (a, s))
--    (DumbTerm f) <*> (DumbTerm a) = DumbTerm (f <*> a)

-- instance (Total m, Monad m) => Monad (DumbTerm m) where
--    return = pure
--    x >>= f = DumbTerm $ do x' <- unDumbTerm x
--                            x'' <- unDumbTerm (f x')
--                            return x''

data Actions
newtype Terminal = Terminal Int

data TermPos = TermPos {termRow,termCol :: !Int}
             deriving Show
data TermRows = TermRows {rowLengths :: !(Map.IntMap Int), lastRow :: !Int}
              deriving Show

type PosixT m = ReaderT Handles m
data Handles = Handles {hIn, hOut :: Int
                        , closeHandles :: IO ()}


-- instance Total (ReaderT Handles m)

-- newtype Draw m a = Draw {unDraw :: (ReaderT Actions
--                                      (ReaderT Terminal
--                                        (StateT TermRows
--                                          (StateT TermPos
--                                            (PosixT m))))) a}
--     deriving (Functor
--               -- , Applicative , Monad
--               , MonadIO
--               , MonadThrow
--               , MonadMask
--               , MonadCatch
--               , MonadReader Actions, MonadReader Terminal, MonadState TermPos,
--               MonadState TermRows, MonadReader Handles)

-- -- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
-- liftReaderT :: m a -> ReaderT r m a
-- liftReaderT m = ReaderT (const m)
-- {-# INLINE liftReaderT #-}
  
-- instance (Total m, Monad m) => Applicative (Draw m) where
--   pure a = Draw ((liftReaderT . pure) a)
--   (Draw f) <*> (Draw a) = Draw (f <*> a) 

-- instance (Total m, Monad m) => Monad (Draw m) where
--   return = pure 
--   m >>= f = Draw $ do m' <- unDraw m
--                       m'' <- unDraw (f m')
--                       return m''
                      

-- instance MonadTrans Draw where
--     -- lift :: m a -> Draw m a
--     lift = Draw . lift . lift . lift . lift . lift
