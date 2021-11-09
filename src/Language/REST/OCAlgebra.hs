{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.REST.OCAlgebra where

data OCAlgebra c a m = OCAlgebra
  {
    isSat  :: c -> m Bool
  , refine :: c -> a -> a -> c
  , top    :: c

  -- For explore optimizations, if not required just make it return 2nd param
  , union  :: c -> c -> c
  -- If not required return False
  , notStrongerThan :: c -> c -> m Bool
  }

fuelOC :: (Monad m) => Int -> OCAlgebra Int a m
fuelOC initFuel = OCAlgebra isSat' refine' initFuel union' notStrongerThan'
  where
    isSat'  c             = return $ c >= 0
    refine' c _ _         = c - 1
    union'  c c'          = max c c'
    notStrongerThan' c c' = return $ c >= c'

contramap :: forall c a b m .
     (b -> a)
  -> OCAlgebra c a m
  -> OCAlgebra c b m
contramap f aoc = aoc{refine = refine'}
  where
    refine' :: c -> b -> b -> c
    refine' c t1 t2 = refine aoc c (f t1) (f t2)
