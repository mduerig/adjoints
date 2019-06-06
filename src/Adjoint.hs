{-# LANGUAGE MultiParamTypeClasses #-}

module Adjoint
    ( adjointMain
    ) where

class (Functor l, Functor r) => Adjoint l r where

    -- Unit and counit from natural isomorphism of hom-sets
    unit :: d -> r (l d)
    unit = phiRight id

    counit :: l (r c) -> c
    counit = phiLeft id

    -- Natural isomorphism of hom-sets from unit and counit
    phiLeft :: (d -> r c) -> (l d -> c)
    phiLeft f = counit . fmap f

    phiRight :: (l d -> c) -> (d -> r c)
    phiRight f = fmap f . unit

-- Triangular identities
triLeft :: Adjoint l r => (r (l d) -> l (r (l d))) -> (d -> l d)
triLeft f = counit . f . unit

triRight :: Adjoint l r => (c -> r c) -> (l (r c) -> r (l (r c)))
triRight f = unit . f . counit

adjointMain :: IO ()
adjointMain = putStrLn "adjointMain"
