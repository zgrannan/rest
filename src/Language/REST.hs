-- |

module Language.REST where

import Language.REST.OCAlgebra (OCAlgebra(..))
import Language.REST.RPO (rpoSMT)
import Language.REST.RuntimeTerm
import Language.REST.SMT
import System.IO (Handle)


-- | An ordering constraint algebra derived from the recursive path ordering,
--   with constraints represented directly as SMT expressions.
rpoOCA :: (Handle, Handle) -> OCAlgebra (SMTExpr Bool) RuntimeTerm IO
rpoOCA z3 = OCAlgebra
  { isSat           = checkSat' z3
  , refine          = \e t u -> smtAnd e (rpoSMT t u)
  , top             = smtTrue
  , union           = \e1 e2 -> Or [e1, e2]
  , notStrongerThan = \e1 e2 -> checkSat' z3 (Implies e2 e1)
  }
