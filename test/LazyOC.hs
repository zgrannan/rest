module LazyOC where

import Data.Maybe
import Language.REST.OrderingConstraints.Lazy as LC
import Language.REST.OpOrdering

oo = fromJust . parseOO

tests = [
  ("intersect",
    not $ LC.isSatisfiable $ LC.addConstraint (oo "g > f")
    (LC.addConstraint (oo "f > g ^ f > s") LC.noConstraints)
  )
  ]
