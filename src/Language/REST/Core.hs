{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.REST.Core where

import           Language.REST.OCAlgebra
import           Language.REST.RuntimeTerm

-- | 'orient' generates the constraints on an ordering, that ensures that the size of
--            term in the path is less than or equal to the previous one.
orient :: Show oc => OCAlgebra oc RuntimeTerm m -> [RuntimeTerm] -> oc
orient impl ts0 = go (top impl) (zip ts0 (tail ts0))
   where
    go oc []            = oc
    go oc ((t0, t1):ts) = go (refine impl oc t0 t1) ts
