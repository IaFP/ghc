{-# LANGUAGE CPP, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies,
      FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances,
      TypeSynonymInstances, GeneralizedNewtypeDeriving, QuantifiedConstraints #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HSX.XMLGenerator
-- Copyright   :  (c) Niklas Broberg 2008
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, niklas.broberg@chalmers.se
-- Stability   :  experimental
-- Portability :  requires newtype deriving and MPTCs with fundeps
--
-- The class and monad transformer that forms the basis of the literal XML
-- syntax translation. Literal tags will be translated into functions of
-- the GenerateXML class, and any instantiating monads with associated XML
-- types can benefit from that syntax.
-----------------------------------------------------------------------------
module T4809_XMLGenerator where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Cont  (MonadCont)
import Control.Monad.Error (MonadError)
import Control.Monad.Reader(MonadReader)
import Control.Monad.Writer(MonadWriter)
import Control.Monad.State (MonadState)
import Control.Monad.RWS   (MonadRWS)
import Control.Monad (MonadPlus(..),liftM)
import GHC.Types (type (@), Total)
----------------------------------------------
-- General XML Generation

-- | The monad transformer that allows a monad to generate XML values.
newtype (m @ a) => XMLGenT m a = XMLGenT (m a)
  deriving (Functor)

deriving instance (Total m, Monad m) => Monad (XMLGenT m)
deriving instance (Total m, MonadIO m) => MonadIO (XMLGenT m)
deriving instance (Total m, MonadPlus m) => MonadPlus (XMLGenT m)
deriving instance (Total m, MonadWriter w m) => MonadWriter w (XMLGenT m)
deriving instance (Total m, MonadReader w m) => MonadReader w (XMLGenT m)
deriving instance (Total m, MonadState s m) => MonadState s (XMLGenT m)
deriving instance (Total m, MonadRWS r w s m) => MonadRWS r w s (XMLGenT m)
deriving instance (Total m, MonadCont m) => MonadCont (XMLGenT m)
deriving instance (Total m, MonadError e m) => MonadError e (XMLGenT m)

instance (Total m, Monad m) => Applicative (XMLGenT m) where
  pure  = return
  (<*>) = ap

instance (Total m, Monad m) => Alternative (XMLGenT m) where

-- | un-lift.
unXMLGenT :: XMLGenT m a -> m a
unXMLGenT   (XMLGenT ma) =  ma

instance MonadTrans XMLGenT where
 lift = XMLGenT

type Name = (Maybe String, String)

-- | Generate XML values in some XMLGenerator monad.
class (Total m, Monad m) => XMLGen m where
 type XML m
 data Child m
 genElement  :: Name -> [XMLGenT m [Int]] -> [XMLGenT m [Child m]] -> XMLGenT m (XML m)
 genEElement :: Child @ m => Name -> [XMLGenT m [Int]]             -> XMLGenT m (XML m)
 genEElement n ats = genElement n ats []

-- | Embed values as child nodes of an XML element. The parent type will be clear
-- from the context so it is not mentioned.
class XMLGen m => EmbedAsChild m c where
 asChild :: c -> XMLGenT m [Child m]

instance (Total n, MonadIO m, EmbedAsChild m c, m ~ n) => EmbedAsChild m (XMLGenT n c) where
 asChild m = do
      liftIO $ putStrLn "EmbedAsChild m (XMLGenT n c)"
      a <- m
      asChild a

instance (Total m, MonadIO m, EmbedAsChild m c) => EmbedAsChild m [c] where
  asChild cs =
      do liftIO $ putStrLn "EmbedAsChild m [c]"
         liftM concat . mapM asChild $ cs

instance (Total m, MonadIO m, XMLGen m) => EmbedAsChild m (Child m) where
 asChild c =
     do liftIO $ putStrLn "EmbedAsChild m (Child m)"
        return . return $ c
