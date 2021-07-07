{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module ShouldCompile where
import GHC.Types(type (@@))
type Generic i o = forall x. (i @@ x, o @@ x) => i x -> o x
type Id x = x

foo :: Generic Id Id
foo = error "urk"

-- The point here is that we instantiate "i" and "o"
-- with a partially applied type synonym.  This is
-- OK in GHC because we check type validity only *after*
-- expanding type synonyms (with LiberalTypeSynonyms enabled).
--
-- However, a bug in GHC 5.03-Feb02 made this break a
-- type invariant (see Type.mkAppTy)

