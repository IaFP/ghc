## Suffix
```regex
(\w|,|\(|\s|\n)*#if\s+MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif
```

## Instances
```regex
(instance)(\w|,|\(|\s|\n)*#if\s+MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif
```

## Testing
```hs
instance (
#if MIN_VERSION_base(4,16,0)
    WFT (XOverLit (GhcPass p)),
    WFT (XOverLit (GhcPass (NoGhcTcPass p))),
    WFT (Anno (IdGhcP p)),
    WFT (Anno (IdGhcP (NoGhcTcPass p))),
    WFT (SyntaxExprGhc p),
    WFT (SyntaxExprGhc (NoGhcTcPass p)),
#endif
  OutputableBndrId p) => Outputable (HsExpr (GhcPass p)) where
    ppr expr = pprExpr expr
	 
instance 

#if MIN_VERSION_base(4,16,0)
   WFT a
#endif
  => Pfft where
  pfft = undefined
  

instance (
  Foobar a,
#if MIN_VERSION_base(4,16,0)
  WFT 
#endif

instance pfft 
   #if MIN_VERSION_base(4,16,0) 
	asdfasdf
	ddd
	#endif
```

# Class

```regex
(class)(\w|,|\(|\s|\n)*#if\s+MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif
```

## testing

```hs
class 
#if MIN_VERSION_base(4,16,0)
  WFT a => 
#endif
  Pfft a where
  pfft :: Bool
  
class (
  Poo b,
#if MIN_VERSION_base(4,16,0)
  WFT a,
  WFT b,
#endif
  Poo c) => Pfffft a where
  pfffft :: Bool

```


## (Top-level) term signatures
```regex
^(\w+\s+::)(\w|,|\(|\s|\n)*#if\s+MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif
```

**Note**: To catch nested signatures, i.e, from let bindings, GADTs, or records,
remove the `^` above and take the difference.


## Testing
```hs
-----------------------
-----------------------
-- pprExpr, pprLExpr, pprBinds call pprDeeper;
-- the underscore versions do not
pprLExpr :: (
#if MIN_VERSION_base(4,16,0)
    WFT (XOverLit (GhcPass p)),
    WFT (XOverLit (GhcPass (NoGhcTcPass p))),
    WFT (Anno (IdGhcP p)),
    WFT (Anno (IdGhcP (NoGhcTcPass p))),
    WFT (SyntaxExprGhc p),
    WFT (SyntaxExprGhc (NoGhcTcPass p)),
#endif
  OutputableBndrId p) => LHsExpr (GhcPass p) -> SDoc
  
pprLExpr :: (
    Pfft a,
#if MIN_VERSION_base(4,16,0)
    WFT (XOverLit (GhcPass p)),
    WFT (XOverLit (GhcPass (NoGhcTcPass p))),
    WFT (Anno (IdGhcP p)),
    WFT (Anno (IdGhcP (NoGhcTcPass p))),
    WFT (SyntaxExprGhc p),
    WFT (SyntaxExprGhc (NoGhcTcPass p)),
#endif
  Pfft b,
  OutputableBndrId p) => LHsExpr (GhcPass p) -> SDoc
  
pprExpr ::
  #if MIN_VERSION_base(4,16,0)
  Pfft a =>
  #endif
  restOfSignature -> Bool
pprExpr = undefined
  
-- Inside records
data Foo = {
  foobar ::
#if MIN_VERSION_base(4,16,0)
  Pfft a =>
#endif [a]
}

-- GADTs
data Foo where
  foobar ::
#if MIN_VERSION_base(4,16,0)
  Pfft a =>
#endif Foo a
```
