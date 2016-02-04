module Core (permutate) where

import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR, xor)
import Data.Word (Word64)

import Data.Bits.Bitwise (mask)

import Hash (hash64U)

-- LOW LEVEL / CORE
pad :: (Num b, Integral a) => a -> b
pad = fromIntegral

-- | Truncate to n least significant bits
trunc' :: (Bits a, Num a) => Int -> a -> a
trunc' n w = w .&. mask n

-- | Balanced Feistel Network using only the given number of bits
balancedFeistel' :: (Bits a, Integral a, Num b) =>
            Int                 -- number of bits needed to represent every value
         -> (k -> b -> a)       -- keyed pseudo random function
         -> [k]                 -- key schedule
         -> a                   -- block to encrypt
         -> a                   -- cypher
balancedFeistel' size prf keys block | size `mod` 2 == 0
                             = merge' half $ foldl rnd (l, r) keys
  where
    half = size `div` 2
    (l, r) = split' half block
    rnd (l, r) k = (r, l `xor` trunc' half (prf k (pad r)))

-- FIXME replace/ remove max with card - 1 (if necessary)
permutate :: Word64 -> Word64 -> Word64 -> Word64 -> (Word64, Word64, Word64)
permutate seed card max value = if perm > max
                                then permutate seed card' max perm
                                else (perm, card', seed + card')
  where
    numBits n = floor (logBase 2 (fromIntegral n)) + 1
    nextEven n = if n `mod` 2 == 0 then n else n + 1
    size = (nextEven . numBits) max
    keys = take 7 $ tail $ iterate hash64U seed
    enc = balancedFeistel' size (\k v -> hash64U (k + v)) keys
    perm = enc value
    card' = card + 7

split' k n = (n .&. fromIntegral (2^k-1), n `shiftR` k)

merge' k (l, r) = (r `shiftL` k) .|. l
