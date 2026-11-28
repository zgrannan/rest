{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.REST.OCAlgebra where

-- | Ordering Constraint Algebra: Operations to determine if there is a
--   well-quasi ordering that makes a sequence of terms non-increasing.
--
-- @c@ is a type representing ordering constraints.
--
-- @a@ is the type of the terms in a sequence.
--
-- @m@ is a monad that allows to perform computations, such as checking the
-- satisfiability of constraints.
--
data OCAlgebra c a m = OCAlgebra
  { -- | Checks if the constraints are satisfiable
    isSat  :: c -> m Bool
    -- | @refine c t u@ strengthens @c@ to permit @t >= u@
    --
    -- Refine might emit bindings (e.g. constant definitions) to be used in
    -- the satisfiability checks, in order to reduce the size of the
    -- constraints (see https://github.com/zgrannan/rest/issues/16).
  , refine :: c -> a -> a -> m c
    -- | Allows any non-increasing ordering.
  , top :: c
    -- | @union c0 c1@ allows any of the orderings permitted by @c0@ or @c1@.
  , union  :: c -> c -> c
    -- | @c1 `notStrongerThan` c2 == True@ if @c1@ permits all orderings allowed
    -- by @c2@.
  , notStrongerThan :: c -> c -> m Bool
  }

-- | @fuelOC n@ is an OCA that permits @n@ rewrite steps
fuelOC :: (Monad m) => Int -> OCAlgebra Int a m
fuelOC initFuel = OCAlgebra isSat' refine' initFuel union' notStrongerThan'
  where
    isSat'  c             = return $ c >= 0
    refine' c _ _         = return $ c - 1
    union'                = max
    notStrongerThan' c c' = return $ c >= c'

-- | @contramap f oca@ transforms an OCA of terms of type @a@ terms of type @b@,
--   by using @f@ to convert terms of @b@ to equivalent ones of @a@
contramap :: forall c a b m .
     (b -> a)
  -> OCAlgebra c a m
  -> OCAlgebra c b m
contramap f oca = oca{refine = refine'}
  where
    refine' :: c -> b -> b -> m c
    refine' c t1 t2 = refine oca c (f t1) (f t2)

-- | @bimapConstraints to from oca@ yields an oca using @d@ to track constraints; @to@ and @from@ should
--   define an isomorphism between c and d
bimapConstraints :: forall c d a m .
     Functor m
  => (c -> d)
  -> (d -> c)
  -> OCAlgebra c a m
  -> OCAlgebra d a m
bimapConstraints to from oca = OCAlgebra isSat' refine' (to (top oca)) union' notStrongerThan'
  where
    isSat' :: d -> m Bool
    isSat' c = isSat oca (from c)

    refine' :: d -> a -> a -> m d
    refine' c t1 t2 = to <$> refine oca (from c) t1 t2

    union' :: d -> d -> d
    union' c1 c2 = to $ union oca (from c1) (from c2)

    notStrongerThan' :: d -> d -> m Bool
    notStrongerThan' c1 c2 = notStrongerThan oca (from c1) (from c2)
