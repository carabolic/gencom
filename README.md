# Example (envisioned)

```haskell
    data Customer = Customer Int String Int
```

```haskell
    cId :: Generator Int
    cId = unqiue $ range 1 100
```

```íhphaskell
cName :: Generator String
cName = undefined
```

```haskell
cAge :: Generator Int
cAge = random $ range 0 120
```

## Combining indenpendent generators using applicative notation

```haskell
customerA :: Generator Customer
customerA = return $ Customer <$> cId <*> cName <*> cAge
```

## Combining dependent generators using monad notation

```haskell
customerM :: Generator (Int, String)
customerM = do
  custId <- cId
  prefCustId <- "cust" ++ show custId
  return (custId, prefCustId)
```

