-- |

module Language.REST where

import Data.Text (Text)
import Language.REST.OCAlgebra (OCAlgebra(..))
import Language.REST.RPO (rpoSMT)
import Language.REST.LPO (lpoSMT, lpoStrictSMT)
import Language.REST.RuntimeTerm
import Language.REST.SMT


-- | An ordering constraint algebra derived from the recursive path ordering.
rpoOCA :: SolverHandle -> Text -> IO (OCAlgebra (SMTExpr Bool) RuntimeTerm IO)
rpoOCA z3 prefix = smtOCA z3 prefix rpoSMT

-- | An ordering constraint algebra derived from the lexicographic path ordering.
lpoOCA :: SolverHandle -> Text -> IO (OCAlgebra (SMTExpr Bool) RuntimeTerm IO)
lpoOCA z3 prefix = smtOCA z3 prefix lpoSMT

-- | A strict version of 'lpoOCA'.
lpoStrictOCA :: SolverHandle -> Text -> IO (OCAlgebra (SMTExpr Bool) RuntimeTerm IO)
lpoStrictOCA z3 prefix = smtOCA z3 prefix lpoStrictSMT
