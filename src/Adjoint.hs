{-# LANGUAGE MultiParamTypeClasses #-}

module Adjoint
    ( adjointMain
    ) where

class (Functor l, Functor r) => Adjoint l r where
    unit   :: d -> r (l d)
    counit :: l (r c) -> c

-- Natural isomorphism of hom-sets from unit and counit
phiLeft :: Adjoint l r => (l d -> c) -> (d -> r c)
phiLeft f = fmap f . unit

phiRight :: Adjoint l r => (d -> r c) -> (l d -> c)
phiRight f = counit . fmap f

-- Unit and counit from natural isomorphism of hom-sets
unit' :: Adjoint l r => d -> r (l d)
unit' = phiLeft id

counit' :: Adjoint l r => l (r c) -> c
counit' = phiRight id

-- Triangular identities
triLeft :: Adjoint l r => (r (l d) -> l (r (l d))) -> (d -> l d)
triLeft f = counit . f . unit

triRight :: Adjoint l r => (c -> r c) -> (l (r c) -> r (l (r c)))
triRight f = unit . f . counit


adjointMain :: IO ()
adjointMain = putStrLn "adjointMain"
