Examples
========

> data Customer = Customer Int String Int

> cId :: Generator Int
> cId = unqiue $ range 1 100

> cName :: Generator String
> cName = undefined

> cAge :: Generator Int
> cAge = random $ range 0 120

## Combining indenpendent generators using applicative notation

customerA :: Generator Customer
customerA = return $ Customer <$> cId <*> cName <*> cAge

## Combining dependent generators using monad notation

customerM :: Generator (Int, String)
customerM = do
  custId <- cId
  prefCustId <- "cust" ++ show custId
  return (custId, prefCustId)
