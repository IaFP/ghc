module GHC.Types.Id.Make where
import GHC.Types.Name( Name )
import GHC.Types.Var( Id )
import GHC.Core.Class( Class )
import GHC.Core.Type ( Type )
import GHC.Types.Unique.Supply (UniqSupply)
import {-# SOURCE #-} GHC.Core.DataCon( DataCon, DataConRep )
import {-# SOURCE #-} GHC.Builtin.PrimOps( PrimOp )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
updateDataConRep :: UniqSupply -> DataCon -> DataConRep -> [Type] -> Type -> DataConRep

mkDictSelId     :: Name -> Class   -> Id

mkPrimOpId      :: PrimOp -> Id
voidPrimId      :: Id
