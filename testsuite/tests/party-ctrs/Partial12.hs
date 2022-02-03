



class (T a, B b c, ... ) => Cls a b c where
  m1 :: (O d e, ... ) => d -> e -> f
  m2 :: (O' f g, ... ) => f -> g -> e
  
instance (TyCtx a b, ...) => Cls (G a b c)
  m1 = some def
  -- default m2 -> Cls.$dmm2 @(G a b c) 

