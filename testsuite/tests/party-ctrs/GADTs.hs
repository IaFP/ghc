module GADTs where

import GHC.Types

data Nat :: Type -> Type where
  Z :: Nat Int
  S :: Nat Int -> Nat Int

eval :: Nat a -> a
eval Z = 0
eval (S x) =  1 + eval x

{-

eval
  = \ (@a_a3tZ)
      (irred_a3u0 :: Nat @ a_a3tZ)
      (ds_d3uI :: Nat a_a3tZ) ->
      case ds_d3uI of {
        Z co_a3u1 ->
          fromInteger
            @a_a3tZ
            (GHC.Num.$fNumInt
             `cast` ((Num (Sym co_a3u1))_R :: Num Int ~R# Num a_a3tZ))
            (GHC.Num.Integer.IS 0#);
        S co_a3u7 x_axk ->
          + @a_a3tZ
            (GHC.Num.$fNumInt
             `cast` ((Num (Sym co_a3u7))_R :: Num Int ~R# Num a_a3tZ))
            (fromInteger
               @a_a3tZ
               (GHC.Num.$fNumInt
                `cast` ((Num (Sym co_a3u7))_R :: Num Int ~R# Num a_a3tZ))
               (GHC.Num.Integer.IS 1#))
            (eval
               @a_a3tZ
               irred_a3u0
               (x_axk `cast` ((Nat (Sym co_a3u7))_R :: Nat Int ~R# Nat a_a3tZ)))
      }


-}
