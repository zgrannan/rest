-- |

module Language.REST where

import Language.REST.OCAlgebra (OCAlgebra(..))
import Language.REST.RPO (rpoSMT)
import Language.REST.LPO (lpoSMT, lpoStrictSMT)
import Language.REST.RuntimeTerm
import Language.REST.SMT
import System.IO (Handle)


-- | Builds an ordering constraint algebra from a direct SMT step function.
smtOCA
  :: (Handle, Handle)
  -> (RuntimeTerm -> RuntimeTerm -> SMTExpr Bool)
  -> OCAlgebra (SMTExpr Bool) RuntimeTerm IO
smtOCA z3 step = OCAlgebra
  { isSat           = checkSat' z3
  , refine          = \e t u -> smtAnd e (step t u)
  , top             = smtTrue
  , union           = \e1 e2 -> Or [e1, e2]
  , notStrongerThan = \e1 e2 -> checkSat' z3 (Implies e2 e1)
  }

-- | An ordering constraint algebra derived from the recursive path ordering.
rpoOCA :: (Handle, Handle) -> OCAlgebra (SMTExpr Bool) RuntimeTerm IO
rpoOCA z3 = smtOCA z3 rpoSMT

-- | An ordering constraint algebra derived from the lexicographic path ordering.
lpoOCA :: (Handle, Handle) -> OCAlgebra (SMTExpr Bool) RuntimeTerm IO
lpoOCA z3 = smtOCA z3 lpoSMT

-- | A strict version of 'lpoOCA'.
lpoStrictOCA :: (Handle, Handle) -> OCAlgebra (SMTExpr Bool) RuntimeTerm IO
lpoStrictOCA z3 = smtOCA z3 lpoStrictSMT
