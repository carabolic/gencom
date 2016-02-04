module Hash where

import Data.Word
import Data.Int

hash32 :: Int32 -> Int32
hash32 key = fromIntegral $ hash_wang_32 (fromIntegral key)

hash64 :: Int64 -> Int64
hash64 key = fromIntegral $ hash_wang_64 (fromIntegral key)

hash64U :: Word64 -> Word64
hash64U = hash_wang_64

foreign import ccall unsafe "wang.c hashable_wang_32" hash_wang_32
    :: Word32 -> Word32
foreign import ccall unsafe "wang.c hashable_wang_64" hash_wang_64
    :: Word64 -> Word64
