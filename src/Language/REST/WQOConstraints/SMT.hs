{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.REST.WQOConstraints.SMT
  ( SMTConstraint(..)
  , smtOC
  ) where

import Data.Hashable
import Language.REST.SMT
import qualified Language.REST.Internal.WQO as WQO
import qualified Language.REST.WQOConstraints as OC

-- | An ordering constraint represented directly as an SMT expression. The type
--   parameter is only used so this type can serve as the @oc@ of 'OC.WQOConstraints'.
newtype SMTConstraint a = SMTConstraint { unSMTConstraint :: SMTExpr Bool }
  deriving (Eq, Ord, Show)

instance Hashable (SMTConstraint a) where
  hashWithSalt salt (SMTConstraint e) = hashWithSalt salt e

instance ToSMT (SMTConstraint a) Bool where
  toSMT = unSMTConstraint

-- | An SMT-expression-based implementation of ordering constraints.
smtOC :: SolverHandle -> OC.WQOConstraints SMTConstraint IO
smtOC z3 = OC.OC
  addConstraint
  intersect
  (checkSat' z3 . unSMTConstraint)
  (notStrongerThan z3)
  noConstraints
  permits
  union
  unsatisfiable
  getOrdering

addConstraint :: (ToSMTVar a Int, Eq a, Ord a, Hashable a) => WQO.WQO a -> SMTConstraint a -> SMTConstraint a
addConstraint wqo c = SMTConstraint (smtAnd [unSMTConstraint c, toSMT wqo])

intersect :: SMTConstraint a -> SMTConstraint a -> SMTConstraint a
intersect (SMTConstraint e1) (SMTConstraint e2) = SMTConstraint (smtAnd [e1, e2])

union :: SMTConstraint a -> SMTConstraint a -> SMTConstraint a
union (SMTConstraint e1) (SMTConstraint e2) = SMTConstraint (smtOr [e1, e2])

noConstraints :: SMTConstraint a
noConstraints = SMTConstraint smtTrue

unsatisfiable :: SMTConstraint a
unsatisfiable = SMTConstraint smtFalse

notStrongerThan :: SolverHandle -> SMTConstraint a -> SMTConstraint a -> IO Bool
notStrongerThan z3 t1 t2 = checkSat' z3 (smtImplies (unSMTConstraint t2) (unSMTConstraint t1))

permits :: SMTConstraint a -> WQO.WQO a -> Bool
permits _ _ = error "unreachable: SMTConstraint has no WQO extraction"

getOrdering :: SMTConstraint a -> Maybe (WQO.WQO a)
getOrdering _ = Nothing
