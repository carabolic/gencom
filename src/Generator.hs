module Generator where

import Data.Word (Word64)

import Core (permutate)
import Hash (hash64U)

data Generator a = Generator { card :: Word64
                             , unrank :: Word64 -> a
                             , runGen :: Word64 -> Word64 -> a }

instance Functor Generator where
  fmap f (Generator c u rg) = Generator c (f . u) (\s n -> f $ rg s n)

instance Applicative Generator where
  pure x = Generator 1 (const x) (const $ const x)
  Generator cl ul rgl <*> Generator cr ur rgr = Generator c u rg 
    where
      c = cl * cr
      u n = ul n (ur n)
      rg s n = rgl s n (rgr s n)

instance Monad Generator where
  Generator c u rg >>= f = Generator c ub rgb
    where
      Generator cb ub rgb = f (u c)

-- COMBINATORS

-- | Generate value v, where lb <= v <= ub
range :: Enum a => a -> a -> Generator a
range lb ub = Generator c u rg
  where
    c = fromIntegral $ fromEnum ub - fromEnum lb + 1
    u = toEnum . (+) (fromEnum lb) . (\n -> n `mod` fromIntegral c) . fromIntegral
    rg seed value = u $ fromIntegral $ seed + value

-- nullable :: Eq a => Generator a -> Generator (Maybe a)
-- nullable (Generator c u rg) = Generator c' u' rg'
--   where
--     c' = c + 1
--     u' n = if n `mod` c' == 0 then Nothing else Just $ u (n - 1) 
--     -- u' n = Just $ u (n - 1)
--     rg' s n = if n `mod` c' == 0 then Nothing else Just $ rg s (n - 1)

random :: Generator a -> Generator a
random gen = gen {runGen = (\k v -> unrank gen $ hash64U (k + v))}

unique :: Generator a -> Generator a
unique (Generator c u rg) = Generator c u rg'
  where
    rg' seed value | value < c = u $ case permutate seed c c value of (v, s', c') -> v
                   | otherwise = error "To large"


generate :: Word64 -> Generator a -> [a]
generate seed gen = map (runGen gen seed) [0..]
