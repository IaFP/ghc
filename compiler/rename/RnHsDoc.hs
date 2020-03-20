{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
#endif

module RnHsDoc ( rnHsDoc, rnLHsDoc, rnMbLHsDoc ) where

import GhcPrelude

import TcRnTypes
import GHC.Hs
import SrcLoc


rnMbLHsDoc :: Maybe LHsDocString -> RnM (Maybe LHsDocString)
rnMbLHsDoc mb_doc = case mb_doc of
  Just doc -> do
    doc' <- rnLHsDoc doc
    return (Just doc')
  Nothing -> return Nothing

rnLHsDoc :: LHsDocString -> RnM LHsDocString
rnLHsDoc (dL->L pos doc) = do
  doc' <- rnHsDoc doc
  return (cL pos doc')

rnHsDoc :: HsDocString -> RnM HsDocString
rnHsDoc = pure
