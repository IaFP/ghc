module MkId where
import Name( Name )
import Var( Id )
import Class( Class )
import Type ( Type )
import UniqSupply (UniqSupply)
import {-# SOURCE #-} DataCon( DataCon, DataConRep )
import {-# SOURCE #-} PrimOp( PrimOp )

data DataConBoxer

mkDataConWorkId  :: Name -> DataCon -> Id
mkDictSelId      :: Name -> Class   -> Id
updateDataConRep :: UniqSupply -> DataCon -> DataConRep -> [Type] -> Type -> DataConRep

mkPrimOpId       :: PrimOp -> Id

magicDictId :: Id
