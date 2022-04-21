{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash,RoleAnnotations, UndecidableInstances, QuantifiedConstraints #-}

module DataFamilies where

import Data.Proxy
import Data.Kind

import Control.Applicative
import Control.Monad        (MonadPlus)
import Control.Monad.Trans  (MonadTrans(lift), MonadIO(liftIO))
import GHC.Types (type (@), Total)


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
class Monad m => XMLGen m where
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

data DXML
  = Element Name [Int] [DXML] | CDATA Bool String
    deriving Show

-- * IdentityT Monad Transformer

newtype m @ a => IdentityT m a = IdentityT { runIdentityT :: m a }
    deriving (Functor)

instance (Total m, Monad m) => Monad (IdentityT m)
instance (Total m, MonadIO m) => MonadIO (IdentityT m)
instance (Total m, MonadPlus m) => MonadPlus (IdentityT m)

instance (Total m, Monad m) => Applicative (IdentityT m) where
instance (Total m, Monad m) => Alternative (IdentityT m) where

instance MonadTrans IdentityT where
    lift = IdentityT

evalIdentityT :: (Total m, Functor m, Monad m) => XMLGenT (IdentityT m) DXML -> m DXML
evalIdentityT = runIdentityT . unXMLGenT

-- * HSX.XMLGenerator for IdentityT

instance (Total m, Functor m, Monad m) => XMLGen (IdentityT m) where
    type XML (IdentityT m) = DXML
    newtype Child (IdentityT m) = IChild { unIChild :: DXML }
    genElement n _attrs children = XMLGenT $ 
                                  do children' <- unXMLGenT (fmap (map unIChild . concat) (sequence children))
                                     return (Element n [] children')

instance (Total m, Monad m, MonadIO m, Functor m) => EmbedAsChild (IdentityT m) String where
    asChild s = 
        do liftIO $ putStrLn "EmbedAsChild (IdentityT m) String"
           XMLGenT . return . (:[]) . IChild . CDATA True $ s
