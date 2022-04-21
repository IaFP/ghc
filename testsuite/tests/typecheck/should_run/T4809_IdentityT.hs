{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, QuantifiedConstraints #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module T4809_IdentityT
    ( evalIdentityT
    , IdentityT(..)
    , XML(..)
    ) where

import Control.Applicative
import Control.Monad        (MonadPlus)
import Control.Monad.Trans  (MonadTrans(lift), MonadIO(liftIO))
import T4809_XMLGenerator (XMLGenT(..), EmbedAsChild(..), Name)
import qualified T4809_XMLGenerator as HSX
import GHC.Types (type (@), Total)

data XML
  = Element Name [Int] [XML] | CDATA Bool String
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

evalIdentityT :: (Total m, Functor m, Monad m) => XMLGenT (IdentityT m) XML -> m XML
evalIdentityT = runIdentityT . HSX.unXMLGenT

-- * HSX.XMLGenerator for IdentityT

instance (Total m, Functor m, Monad m) => HSX.XMLGen (IdentityT m) where
    type XML (IdentityT m) = XML
    newtype Child (IdentityT m) = IChild { unIChild :: XML }
    genElement n _attrs children = HSX.XMLGenT $ 
                                  do children' <- HSX.unXMLGenT (fmap (map unIChild . concat) (sequence children))
                                     return (Element n [] children')

instance (Total m, Monad m, MonadIO m, Functor m) => EmbedAsChild (IdentityT m) String where
    asChild s = 
        do liftIO $ putStrLn "EmbedAsChild (IdentityT m) String"
           XMLGenT . return . (:[]) . IChild . CDATA True $ s
