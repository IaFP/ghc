{-# LANGUAGE DeriveFunctor, DerivingVia, RankNTypes #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators, UndecidableSuperClasses #-}
#endif
-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2010
--
-- The Session type and related functionality
--
-- -----------------------------------------------------------------------------

module GHC.Driver.Monad (
        -- * 'Ghc' monad stuff
        GhcMonad(..),
        Ghc(..),
        GhcT(..), liftGhcT,
        reflectGhc, reifyGhc,
        getSessionDynFlags,
        liftIO,
        Session(..), withSession, modifySession, modifySessionM,
        withTempSession,

        -- * Logger
        modifyLogger,
        pushLogHookM,
        popLogHookM,
        putLogMsgM,
        putMsgM,
        withTimingM,

        -- ** Diagnostics
        logDiagnostics, printException,
        WarnErrLogger, defaultWarnErrLogger
  ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Env
import GHC.Driver.Errors ( printOrThrowDiagnostics, printMessages )
import GHC.Driver.Errors.Types
import GHC.Driver.Config.Diagnostic

import GHC.Utils.Monad
import GHC.Utils.Exception
import GHC.Utils.Error
import GHC.Utils.Logger

import GHC.Types.SrcLoc
import GHC.Types.SourceError

import Control.Monad
import Control.Monad.Catch as MC
import Control.Monad.Trans.Reader
import Data.IORef
#if MIN_VERSION_base(4,16,0)
import GHC.Types (type(@), Total)
import Control.Monad.Trans.Class (MonadTrans, lift)
#endif

-- -----------------------------------------------------------------------------
-- | A monad that has all the features needed by GHC API calls.
--
-- In short, a GHC monad
--
--   - allows embedding of IO actions,
--
--   - can log warnings,
--
--   - allows handling of (extensible) exceptions, and
--
--   - maintains a current session.
--
-- If you do not use 'Ghc' or 'GhcT', make sure to call 'GHC.initGhcMonad'
-- before any call to the GHC API functions can occur.
--
class (
#if MIN_VERSION_base(4,16,0)
    Total m,
#endif 
  Functor m, ExceptionMonad m, HasDynFlags m, HasLogger m ) => GhcMonad m where
  getSession :: m HscEnv
  setSession :: HscEnv -> m ()

-- | Call the argument with the current session.
withSession :: (GhcMonad m) => (HscEnv -> m a) -> m a
withSession f = getSession >>= f

-- | Grabs the DynFlags from the Session
getSessionDynFlags :: GhcMonad m => m DynFlags
getSessionDynFlags = withSession (return . hsc_dflags)

-- | Set the current session to the result of applying the current session to
-- the argument.
modifySession :: GhcMonad m => (HscEnv -> HscEnv) -> m ()
modifySession f = do h <- getSession
                     setSession $! f h

-- | Set the current session to the result of applying the current session to
-- the argument.
modifySessionM :: GhcMonad m => (HscEnv -> m HscEnv) -> m ()
modifySessionM f = do h <- getSession
                      h' <- f h
                      setSession $! h'

withSavedSession :: GhcMonad m => m a -> m a
withSavedSession m = do
  saved_session <- getSession
  m `MC.finally` setSession saved_session

-- | Call an action with a temporarily modified Session.
withTempSession :: GhcMonad m => (HscEnv -> HscEnv) -> m a -> m a
withTempSession f m =
  withSavedSession $ modifySession f >> m

----------------------------------------
-- Logging
----------------------------------------

-- | Modify the logger
modifyLogger :: GhcMonad m => (Logger -> Logger) -> m ()
modifyLogger f = modifySession $ \hsc_env ->
    hsc_env { hsc_logger = f (hsc_logger hsc_env) }

-- | Push a log hook on the stack
pushLogHookM :: GhcMonad m => (LogAction -> LogAction) -> m ()
pushLogHookM = modifyLogger . pushLogHook

-- | Pop a log hook from the stack
popLogHookM :: GhcMonad m => m ()
popLogHookM  = modifyLogger popLogHook

-- | Put a log message
putMsgM :: GhcMonad m => SDoc -> m ()
putMsgM doc = do
    logger <- getLogger
    liftIO $ putMsg logger doc

-- | Put a log message
putLogMsgM :: GhcMonad m => MessageClass -> SrcSpan -> SDoc -> m ()
putLogMsgM msg_class loc doc = do
    logger <- getLogger
    liftIO $ logMsg logger msg_class loc doc

-- | Time an action
withTimingM :: GhcMonad m => SDoc -> (b -> ()) -> m b -> m b
withTimingM doc force action = do
    logger <- getLogger
    withTiming logger doc force action

-- -----------------------------------------------------------------------------
-- | A monad that allows logging of diagnostics.

logDiagnostics :: GhcMonad m => Messages GhcMessage -> m ()
logDiagnostics warns = do
  dflags <- getSessionDynFlags
  logger <- getLogger
  let !diag_opts = initDiagOpts dflags
  liftIO $ printOrThrowDiagnostics logger diag_opts warns

-- -----------------------------------------------------------------------------
-- | A minimal implementation of a 'GhcMonad'.  If you need a custom monad,
-- e.g., to maintain additional state consider wrapping this monad or using
-- 'GhcT'.
newtype Ghc a = Ghc { unGhc :: Session -> IO a }
  deriving (Functor)
  deriving (MonadThrow, MonadCatch, MonadMask) via (ReaderT Session IO)

-- | The Session is a handle to the complete state of a compilation
-- session.  A compilation session consists of a set of modules
-- constituting the current program or library, the context for
-- interactive evaluation, and various caches.
data Session = Session !(IORef HscEnv)

instance Applicative Ghc where
  pure a = Ghc $ \_ -> return a
  g <*> m = do f <- g; a <- m; return (f a)

instance Monad Ghc where
  return a = Ghc $ \_ -> return a
  m >>= g  = Ghc $ \s -> do a <- unGhc m s; unGhc (g a) s

instance MonadIO Ghc where
  liftIO ioA = Ghc $ \_ -> ioA

instance MonadFix Ghc where
  mfix f = Ghc $ \s -> mfix (\x -> unGhc (f x) s)

instance HasDynFlags Ghc where
  getDynFlags = getSessionDynFlags

instance HasLogger Ghc where
  getLogger = hsc_logger <$> getSession

instance GhcMonad Ghc where
  getSession = Ghc $ \(Session r) -> readIORef r
  setSession s' = Ghc $ \(Session r) -> writeIORef r s'

-- | Reflect a computation in the 'Ghc' monad into the 'IO' monad.
--
-- You can use this to call functions returning an action in the 'Ghc' monad
-- inside an 'IO' action.  This is needed for some (too restrictive) callback
-- arguments of some library functions:
--
-- > libFunc :: String -> (Int -> IO a) -> IO a
-- > ghcFunc :: Int -> Ghc a
-- >
-- > ghcFuncUsingLibFunc :: String -> Ghc a -> Ghc a
-- > ghcFuncUsingLibFunc str =
-- >   reifyGhc $ \s ->
-- >     libFunc $ \i -> do
-- >       reflectGhc (ghcFunc i) s
--
reflectGhc :: Ghc a -> Session -> IO a
reflectGhc m = unGhc m

-- > Dual to 'reflectGhc'.  See its documentation.
reifyGhc :: (Session -> IO a) -> Ghc a
reifyGhc act = Ghc $ act

-- -----------------------------------------------------------------------------
-- | A monad transformer to add GHC specific features to another monad.
--
-- Note that the wrapped monad must support IO and handling of exceptions.
newtype
#if MIN_VERSION_base(4,16,0)
  m @ a => 
#endif
  GhcT m a = GhcT { unGhcT :: Session -> m a }
  deriving (Functor)  
#if MIN_VERSION_base(4,16,0)
instance MonadTrans GhcT where
  lift x = GhcT $ const x
  
instance (Total m,  MonadThrow m) => MonadThrow (GhcT m) where
  throwM e = lift $ throwM e

instance (Total m, MonadCatch m) => MonadCatch (GhcT m) where
  catch (GhcT m) c = GhcT $ \r -> m r `MC.catch` \e -> unGhcT (c e) r

instance (Total m, MonadMask m) => MonadMask (GhcT m) where
  mask a = GhcT $ \e -> MC.mask $ \u -> unGhcT (a $ q u) e
    where q :: (m a -> m a) -> GhcT m a -> GhcT m a
          q u (GhcT b) = GhcT (u . b)
  uninterruptibleMask a =
    GhcT $ \e -> MC.uninterruptibleMask $ \u -> unGhcT (a $ q u) e
      where q :: (m a -> m a) -> GhcT m a -> GhcT m a
            q u (GhcT b) = GhcT (u . b)

  generalBracket acquire release use = GhcT $ \r ->
    generalBracket
      (unGhcT acquire r)
      (\resource exitCase -> unGhcT (release resource exitCase) r)
      (\resource -> unGhcT (use resource) r)
#else
  deriving (MonadThrow, MonadCatch, MonadMask) via (ReaderT Session m)
#endif
                                                    
liftGhcT :: m a -> GhcT m a
liftGhcT m = GhcT $ \_ -> m

instance Applicative m => Applicative (GhcT m) where
  pure x  = GhcT $ \_ -> pure x
  g <*> m = GhcT $ \s -> unGhcT g s <*> unGhcT m s

instance (Monad m) => Monad (GhcT m) where
  return x  = GhcT $ \_ -> return x
  m >>= k  = GhcT $ \s -> do a <- unGhcT m s; unGhcT (k a) s

instance (
#if MIN_VERSION_base(4,16,0)
    Total m,
#endif
  MonadIO m) => MonadIO (GhcT m) where
  liftIO ioA = GhcT $ \_ -> liftIO ioA

instance (
#if MIN_VERSION_base(4,16,0)
    Total m,
#endif
  MonadIO m) => HasDynFlags (GhcT m) where
  getDynFlags = GhcT $ \(Session r) -> liftM hsc_dflags (liftIO $ readIORef r)

instance (
#if MIN_VERSION_base(4,16,0)
    Total m,
#endif
  MonadIO m) => HasLogger (GhcT m) where
  getLogger = GhcT $ \(Session r) -> liftM hsc_logger (liftIO $ readIORef r)

instance (
#if MIN_VERSION_base(4,16,0)
    Total m,
#endif
  ExceptionMonad m) => GhcMonad (GhcT m) where
  getSession = GhcT $ \(Session r) -> liftIO $ readIORef r
  setSession s' = GhcT $ \(Session r) -> liftIO $ writeIORef r s'


-- | Print the all diagnostics in a 'SourceError'.  Useful inside exception
--   handlers.
printException :: (HasLogger m, MonadIO m, HasDynFlags m) => SourceError -> m ()
printException err = do
  dflags <- getDynFlags
  logger <- getLogger
  let !diag_opts = initDiagOpts dflags
  liftIO $ printMessages logger diag_opts (srcErrorMessages err)

-- | A function called to log warnings and errors.
type WarnErrLogger = forall m. (Applicative m, HasDynFlags m , MonadIO m, HasLogger m)
                               => Maybe SourceError -> m ()

defaultWarnErrLogger :: WarnErrLogger
defaultWarnErrLogger Nothing  = return ()
defaultWarnErrLogger (Just e) = printException e
