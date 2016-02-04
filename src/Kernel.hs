module Kernel where

import Data.Word (Word64)

import Core (permutate)
import Hash (hash64U)

-- |Generates a random value using a Hash function `Word64 -> Word64`.
random :: Word64 -- ^ Seed
       -> Word64 -- ^ Index
       -> Word64 -- ^ Cardinality
       -> (Word64 -> a) -- ^ Unrank funtion
       -> a -- ^ Random value
random s i c unrank = unrank $ hash s i `mod` c
  where hash k v = hash64U (k + v)

-- |Generates a unique random value for each i, given 0 <= i < cardinality.
unique :: Word64 -- ^ Seed
       -> Word64 -- ^ Index
       -> Word64 -- ^ Cardinality
       -> (Word64 -> a) -- ^ Unrank function
       -> Maybe a
unique s i c unrank | i < c = Just $ unrank $ first (permutate s c (c - 1) i) `mod` c
                    | otherwise = Nothing

-- |Generates a random value, where ascending i u <= ascending j u iff i <= j
ascending :: Ord a => Word64 -- ^ Index
          -> (Word64 -> a) -- ^ Unrank function
          -> a
ascending i unrank = unrank i

descending :: Ord a => Word64 -- ^ Index
           -> Word64 -- ^ Cardinality
           -> (Word64 -> a) -- ^ unrank
           -> a
descending i c unrank = unrank $ c - 1 - i

first :: (a, b, c) -> a
first (a, _, _) = a

