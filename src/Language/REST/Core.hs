{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Core REST functions
module Language.REST.Core where

import           Language.REST.OCAlgebra
import           Language.REST.RuntimeTerm

-- | @orient impl ts@ generates the constraints on an ordering defined by the
--   OCA `impl`, that ensures each term in the path `ts` is smaller than or
--   equal to the previous one.
orient :: Monad m => OCAlgebra oc RuntimeTerm m -> [RuntimeTerm] -> m oc
orient impl ts0 = go (top impl) (zip ts0 (tail ts0))
   where
    go oc []            = pure oc
    go oc ((t0, t1):ts) = refine impl oc t0 t1 >>= \c -> go c ts
