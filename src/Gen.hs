module Gen where

import Data.Word (Word64)

import Core (permutate)
import Hash (hash64U)

-- runGen current_seed 
newtype Gen a = Gen {runGen :: Word64 -> Word64 -> Word64 -> (a, Word64, Word64)}

instance Functor Gen where
  f `fmap` g = Gen (\c s v -> let (x, c', s') = runGen g c s v
                              in (f x, c', s'))


instance Applicative Gen where
  pure x = Gen (\c s v -> (x, 0, s))
  -- Gen (a -> b) <*> Gen a -> Gen b
  gf <*> ga = Gen (\c s v -> let (f, c1, s1) = runGen gf c s v
                                 (a, c2, s2) = runGen ga (c1 + c) (s + c1) v
                             in (f a, c2, s2))

instance Monad Gen where
  g >>= f = Gen (\c s v -> let (x, c', s') = runGen g c s v
                               gb = f x
                           in runGen gb c' s' v)
    -- Gen (\c s v -> runGen (f (run c s v)) c s v)

range :: Enum a => a -> a -> Gen a
range l u = Gen (\c s p -> let card = rank u - rank l + 1
                               p' = p `mod` card
                               v = unrank (rank l + p')
                           in (v, card, s))
  where
    rank   = fromIntegral . fromEnum
    unrank = toEnum . fromIntegral

-- given cardinality c, current starting pos s, and current position in the sequence v
-- the actual position in the random sequence is p = s + v `mod` c
-- a = hash64U p `mod` c
-- (a, c, s + c)
random :: Gen a -> Gen a
random g = Gen (\c s v -> runGen g c s (hash64U (s + v)))
-- random g = Gen (\s v -> let (_, c, s') = runGen g s v
--                             p = s + v `mod` c
--                             a = hash64U (p `mod` c)
--                         in (a, c, s' + c))

random' :: Gen a -> Gen a
random' g = Gen (\c s v -> let (_, c1, s1) = runGen g c s v
                               unrank = runGen g c s
                               v' = s1 + (v `mod` c1)
                               s' = s1 + c1
                               (a, _, _) = unrank (hash64U v')
                           in (a, c1, s'))

unique :: Gen a -> Gen a
unique g = Gen (\c s v -> let (_, c', s') = runGen g c s v
                              unrank = runGen g c s
                              (v', s'', c'') = permutate s' 0 (c' - 1) (v `mod` c')
                              (a', _, _) = unrank v'
                          in (a', s'', c''))

data Stats = Stats { card :: Word64
                   , seed :: Word64
                   , row  :: Word64}
           deriving (Show)

stats :: Gen a -> Gen (a, Stats)
stats g = Gen (\c s v -> let (a, c', s') = runGen g c s v
                         in ((a, Stats c' s' v), c', s'))


generate :: Word64 -> Gen a -> [a]
generate seed gen = let (a, card, s) = myGen 0
                    in a : helper card 1
  where
    myGen = runGen gen 0 seed
    helper c pos | pos < c = let (a, c', s') = myGen pos
                             in a : helper c (pos + 1)
                 | otherwise = []
