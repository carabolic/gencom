{-# LANGUAGE ScopedTypeVariables #-}

module Play where

instance Enum a => Enum (Maybe a) where
  toEnum 0 = Nothing
  toEnum n = Just (toEnum (n - 1))

  fromEnum Nothing = 0
  fromEnum a = maybe 0 (\v -> fromEnum v + 1) a


    
card :: forall t. (Bounded t, Integral t) => t -> Integer
card _ = toInteger (maxBound :: t) - toInteger (minBound :: t) + 1

cardEnum :: forall t. (Bounded t, Enum t) => t -> Integer
cardEnum _ = fromIntegral (fromEnum (maxBound :: t)) - fromIntegral (fromEnum (minBound :: t)) + 1


cardRange :: a -> a -> Integer
cardRange = undefined
