{-# LANGUAGE CPP, RankNTypes, RecordWildCards, GADTs, ScopedTypeVariables, ExplicitNamespaces, TypeOperators #-}
module Lib (serv) where

import GHCi.Run
import GHCi.TH
import GHCi.Message

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Binary

import Text.Printf
import System.Environment (getProgName)
#if MIN_VERSION_base(4,16,0)
import GHC.Types (type (@))
#endif


type MessageHook = Msg -> IO Msg

trace :: String -> IO ()
trace s = getProgName >>= \name -> printf "[%20s] %s\n" name s

serv :: Bool -> MessageHook -> Pipe -> (forall a .
#if MIN_VERSION_base(4,16,0)
                                         IO @ a => 
#endif
                                         IO a -> IO a) -> IO ()
serv verbose hook pipe restore = loop
 where
  loop = do
    when verbose $ trace "reading pipe..."
    Msg msg <- readPipe pipe getMessage >>= hook

    discardCtrlC

    when verbose $ trace ("msg: " ++ (show msg))
    case msg of
      Shutdown -> return ()
      RunTH st q ty loc -> wrapRunTH $ runTH pipe st q ty loc
      RunModFinalizers st qrefs -> wrapRunTH $ runModFinalizerRefs pipe st qrefs
      _other -> run msg >>= reply

  reply :: forall a. (Binary a, Show a) => a -> IO ()
  reply r = do
    when verbose $ trace ("writing pipe: " ++ show r)
    writePipe pipe (put r)
    loop

  -- Run some TH code, which may interact with GHC by sending
  -- THMessage requests, and then finally send RunTHDone followed by a
  -- QResult.  For an overview of how TH works with Remote GHCi, see
  -- Note [Remote Template Haskell] in libraries/ghci/GHCi/TH.hs.
  wrapRunTH :: forall a. (Binary a, Show a) => IO a -> IO ()
  wrapRunTH io = do
    when verbose $ trace "wrapRunTH..."
    r <- try io
    when verbose $ trace "wrapRunTH done."
    when verbose $ trace "writing RunTHDone."
    writePipe pipe (putTHMessage RunTHDone)
    case r of
      Left e
        | Just (GHCiQException _ err) <- fromException e  -> do
           when verbose $ trace ("QFail " ++ show err)
           reply (QFail err :: QResult a)
        | otherwise -> do
           str <- showException e
           when verbose $ trace ("QException " ++ str)
           reply (QException str :: QResult a)
      Right a -> do
        when verbose $ trace "QDone"
        reply (QDone a)

  -- carefully when showing an exception, there might be other exceptions
  -- lurking inside it.  If so, we return the inner exception instead.
  showException :: SomeException -> IO String
  showException e0 = do
     when verbose $ trace "showException"
     r <- try $ evaluate (force (show (e0::SomeException)))
     case r of
       Left e -> showException e
       Right str -> return str

  -- throw away any pending ^C exceptions while we're not running
  -- interpreted code.  GHC will also get the ^C, and either ignore it
  -- (if this is GHCi), or tell us to quit with a Shutdown message.
  discardCtrlC = do
    when verbose $ trace "discardCtrlC"
    r <- try $ restore $ return ()
    case r of
      Left UserInterrupt -> return () >> discardCtrlC
      Left e -> throwIO e
      _ -> return ()
