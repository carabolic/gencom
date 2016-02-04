---
format: markdown+lhs
...

Proofs for the Generator Type
=============================

> import Lib

Functor Laws
------------

> fmap id (Generator c u rg)      = id (Generator c u rg)
> Generator (id c) (id u) (id rg) = Generator c u rg
> Generator c u rg                = Generator c u rg

> fmap (f . g) (Generator c u rg)                    == fmap f . fmap g $ (Generator c u rg)
> Generator c (f . g . u) (\s n -> (f . g) $ rg s n) == fmap f $ Generator c (g . u) (\s n -> g $ rg s n)
> Generator c (f . g . u) (\s n -> (f . g) $ rg s n) == Generator c (f . g . u) (\s n -> f $ g $ rg s n)
> Generator c (f . g . u) (\s n -> f . g $ rg s n)   == Generator c (f . g . u) (\s n -> f . g $ rg s n)

Applicative Laws
----------------

> pure id <*> v == v
> Generator 1 (const id) (const $ const id) <*> Generator c u rg                         == Generator c u rg
> Generator (1 * c) (\n -> const id $ n (u n)) (\s n -> const $ const id $ s n (rg s n)) == Generator c u rg
> Generator c (\n -> u n) (\s n -> rg s n)                                               == Generator c u rg
> Generator c u rg                                                                       == Generator c u rg

> -- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
> --
