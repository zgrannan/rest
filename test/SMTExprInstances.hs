{-# LANGUAGE OverloadedStrings #-}

-- | Exercises the hand-written 'Eq', 'Ord' and 'Hashable' instances of 'SMTExpr'
module SMTExprInstances where

import Data.Hashable (hashWithSalt)
import Language.REST.Op (Op (..))
import Language.REST.SMT
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

eqCase :: String -> SMTExpr a -> SMTExpr a -> Bool -> TestTree
eqCase label x y expected =
  testCase label $ assertBool label $ (x == y) == expected

ordCase :: String -> SMTExpr a -> SMTExpr a -> Ordering -> TestTree
ordCase label x y expected =
  testCase label $ assertBool label $ compare x y == expected

hashEqCase :: String -> SMTExpr a -> SMTExpr a -> TestTree
hashEqCase label x y =
  testCase label $ assertBool label $ hashWithSalt 0 x == hashWithSalt 0 y

hashNeqCase :: String -> SMTExpr a -> SMTExpr a -> TestTree
hashNeqCase label x y =
  testCase label $ assertBool label $ hashWithSalt 0 x /= hashWithSalt 0 y

eqTests :: TestTree
eqTests = testGroup "Eq"
  [ eqCase "And/equal"
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      True
  , eqCase "And/unequal"
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4), Greater (Const 5) (Const 6)])
      False
  , eqCase "Add/equal"
      (smtAdd [Const 1, Const 2])
      (smtAdd [Const 1, Const 2])
      True
  , eqCase "Add/unequal"
      (smtAdd [Const 1, Const 2])
      (smtAdd [Const 1, Const 3])
      False
  , eqCase "Or/equal"
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      True
  , eqCase "Or/unequal"
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtOr [Greater (Const 3) (Const 4), Greater (Const 1) (Const 2)])
      False
  , eqCase "Equal/equal"
      (smtEqual [Const 1, Const 2])
      (smtEqual [Const 1, Const 2])
      True
  , eqCase "Equal/unequal"
      (smtEqual [Const 1, Const 2])
      (smtEqual [Const 1, Const 3])
      False
  , eqCase "Greater/equal"
      (Greater (Const 1) (Const 2))
      (Greater (Const 1) (Const 2))
      True
  , eqCase "Greater/left"
      (Greater (Const 1) (Const 2))
      (Greater (Const 2) (Const 2))
      False
  , eqCase "Greater/right"
      (Greater (Const 1) (Const 2))
      (Greater (Const 1) (Const 3))
      False
  , eqCase "GTE/equal"
      (smtGTE (Const 1) (Const 2))
      (smtGTE (Const 1) (Const 2))
      True
  , eqCase "GTE/unequal"
      (smtGTE (Const 1) (Const 2))
      (smtGTE (Const 1) (Const 3))
      False
  , eqCase "Implies/equal"
      (smtImplies (Greater (Const 1) (Const 2)) (Greater (Const 3) (Const 4)))
      (smtImplies (Greater (Const 1) (Const 2)) (Greater (Const 3) (Const 4)))
      True
  , eqCase "Implies/unequal"
      (smtImplies (Greater (Const 1) (Const 2)) (Greater (Const 3) (Const 4)))
      (smtImplies (Greater (Const 3) (Const 4)) (Greater (Const 1) (Const 2)))
      False
  , eqCase "Var/equal"
      (toSMT (Op "x"))
      (toSMT (Op "x"))
      True
  , eqCase "Var/unequal"
      (toSMT (Op "x"))
      (toSMT (Op "y"))
      False
  , eqCase "Const/equal"
      (Const 1)
      (Const 1)
      True
  , eqCase "Const/unequal"
      (Const 1)
      (Const 2)
      False
    -- catch-all @_ == _ = False@ on 'SMTExpr Bool'
  , eqCase "And/Or"
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      False
  , eqCase "And/Equal"
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtEqual [Const 1, Const 2])
      False
  , eqCase "And/Greater"
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (Greater (Const 1) (Const 2))
      False
  , eqCase "And/GTE"
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtGTE (Const 1) (Const 2))
      False
  , eqCase "Or/Equal"
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtEqual [Const 1, Const 2])
      False
  , eqCase "Equal/Greater"
      (smtEqual [Const 1, Const 2])
      (Greater (Const 1) (Const 2))
      False
  , eqCase "Greater/GTE"
      (Greater (Const 1) (Const 2))
      (smtGTE (Const 1) (Const 2))
      False
  , eqCase "GTE/Implies"
      (smtGTE (Const 1) (Const 2))
      (smtImplies (Greater (Const 1) (Const 2)) (Greater (Const 3) (Const 4)))
      False
    -- catch-all @_ == _ = False@ on 'SMTExpr Int'
  , eqCase "Const/Add"
      (Const 1)
      (smtAdd [Const 1, Const 2])
      False
  , eqCase "Add/Var"
      (smtAdd [Const 1, Const 2])
      (toSMT (Op "x"))
      False
  , eqCase "Var/Const"
      (toSMT (Op "x"))
      (Const 1)
      False
  , eqCase "Var/Add"
      (toSMT (Op "x"))
      (smtAdd [Const 1, Const 2])
      False
  ]

ordTests :: TestTree
ordTests = testGroup "Ord"
  [ ordCase "And/LT"
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4), Greater (Const 5) (Const 6)])
      LT
  , ordCase "And/GT"
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4), Greater (Const 5) (Const 6)])
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      GT
  , ordCase "And/EQ"
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      EQ
  , ordCase "Add/LT"
      (smtAdd [Const 1])
      (smtAdd [Const 1, Const 2])
      LT
  , ordCase "Add/GT"
      (smtAdd [Const 1, Const 2])
      (smtAdd [Const 1])
      GT
  , ordCase "Add/EQ"
      (smtAdd [Const 1, Const 2])
      (smtAdd [Const 1, Const 2])
      EQ
  , ordCase "Or/LT"
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4), Greater (Const 5) (Const 6)])
      LT
  , ordCase "Or/GT"
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4), Greater (Const 5) (Const 6)])
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      GT
  , ordCase "Or/EQ"
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      EQ
  , ordCase "Equal/LT"
      (smtEqual [Const 1, Const 2])
      (smtEqual [Const 1, Const 3])
      LT
  , ordCase "Equal/GT"
      (smtEqual [Const 1, Const 3])
      (smtEqual [Const 1, Const 2])
      GT
  , ordCase "Equal/EQ"
      (smtEqual [Const 1, Const 2])
      (smtEqual [Const 1, Const 2])
      EQ
  , ordCase "Greater/LT"
      (Greater (Const 1) (Const 2))
      (Greater (Const 1) (Const 3))
      LT
  , ordCase "Greater/GT"
      (Greater (Const 1) (Const 3))
      (Greater (Const 1) (Const 2))
      GT
  , ordCase "Greater/EQ"
      (Greater (Const 1) (Const 2))
      (Greater (Const 1) (Const 2))
      EQ
  , ordCase "GTE/LT"
      (smtGTE (Const 1) (Const 2))
      (smtGTE (Const 1) (Const 3))
      LT
  , ordCase "GTE/GT"
      (smtGTE (Const 1) (Const 3))
      (smtGTE (Const 1) (Const 2))
      GT
  , ordCase "GTE/EQ"
      (smtGTE (Const 1) (Const 2))
      (smtGTE (Const 1) (Const 2))
      EQ
  , ordCase "Implies/LT"
      (smtImplies (Greater (Const 1) (Const 2)) (Greater (Const 3) (Const 4)))
      (smtImplies (Greater (Const 1) (Const 2)) (Greater (Const 5) (Const 6)))
      LT
  , ordCase "Implies/GT"
      (smtImplies (Greater (Const 1) (Const 2)) (Greater (Const 5) (Const 6)))
      (smtImplies (Greater (Const 1) (Const 2)) (Greater (Const 3) (Const 4)))
      GT
  , ordCase "Implies/EQ"
      (smtImplies (Greater (Const 1) (Const 2)) (Greater (Const 3) (Const 4)))
      (smtImplies (Greater (Const 1) (Const 2)) (Greater (Const 3) (Const 4)))
      EQ
  , ordCase "Var/LT"
      (toSMT (Op "x"))
      (toSMT (Op "y"))
      LT
  , ordCase "Var/GT"
      (toSMT (Op "y"))
      (toSMT (Op "x"))
      GT
  , ordCase "Var/EQ"
      (toSMT (Op "x"))
      (toSMT (Op "x"))
      EQ
  , ordCase "Const/LT"
      (Const 1)
      (Const 2)
      LT
  , ordCase "Const/GT"
      (Const 2)
      (Const 1)
      GT
  , ordCase "Const/EQ"
      (Const 1)
      (Const 1)
      EQ
    -- catch-all based on 'dataToTag#' (constructor declaration order)
  , ordCase "And<Or"
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      LT
  , ordCase "Or>And"
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      GT
  , ordCase "Equal<GTE"
      (smtEqual [Const 1, Const 2])
      (smtGTE (Const 1) (Const 2))
      LT
  , ordCase "GTE>Equal"
      (smtGTE (Const 1) (Const 2))
      (smtEqual [Const 1, Const 2])
      GT
  , ordCase "Greater<GTE"
      (Greater (Const 1) (Const 2))
      (smtGTE (Const 1) (Const 2))
      LT
  , ordCase "GTE<Implies"
      (smtGTE (Const 1) (Const 2))
      (smtImplies (Greater (Const 1) (Const 2)) (Greater (Const 3) (Const 4)))
      LT
  , ordCase "Add<Var"
      (smtAdd [Const 1, Const 2])
      (toSMT (Op "x"))
      LT
  , ordCase "Var>Add"
      (toSMT (Op "x"))
      (smtAdd [Const 1, Const 2])
      GT
  , ordCase "Var<Const"
      (toSMT (Op "x"))
      (Const 1)
      LT
  , ordCase "Const>Var"
      (Const 1)
      (toSMT (Op "x"))
      GT
  , ordCase "Add<Const"
      (smtAdd [Const 1, Const 2])
      (Const 1)
      LT
  , ordCase "Const>Add"
      (Const 1)
      (smtAdd [Const 1, Const 2])
      GT
  ]

hashTests :: TestTree
hashTests = testGroup "Hashable"
  [ hashEqCase "And/equal"
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
  , hashNeqCase "And/payload"
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4), Greater (Const 5) (Const 6)])
  , hashNeqCase "And/Or"
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
  , hashEqCase "Add/equal"
      (smtAdd [Const 1, Const 2])
      (smtAdd [Const 1, Const 2])
  , hashNeqCase "Add/payload"
      (smtAdd [Const 1, Const 2])
      (smtAdd [Const 1, Const 3])
  , hashEqCase "Or/equal"
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
  , hashNeqCase "Or/payload"
      (smtOr [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtOr [Greater (Const 3) (Const 4), Greater (Const 1) (Const 2)])
  , hashEqCase "Equal/equal"
      (smtEqual [Const 1, Const 2])
      (smtEqual [Const 1, Const 2])
  , hashNeqCase "Equal/payload"
      (smtEqual [Const 1, Const 2])
      (smtEqual [Const 1, Const 3])
  , hashNeqCase "And/Equal"
      (smtAnd [Greater (Const 1) (Const 2), Greater (Const 3) (Const 4)])
      (smtEqual [Const 1, Const 2])
  , hashEqCase "Greater/equal"
      (Greater (Const 1) (Const 2))
      (Greater (Const 1) (Const 2))
  , hashNeqCase "Greater/payload"
      (Greater (Const 1) (Const 2))
      (Greater (Const 1) (Const 3))
  , hashEqCase "GTE/equal"
      (smtGTE (Const 1) (Const 2))
      (smtGTE (Const 1) (Const 2))
  , hashNeqCase "GTE/payload"
      (smtGTE (Const 1) (Const 2))
      (smtGTE (Const 1) (Const 3))
  , hashEqCase "Implies/equal"
      (smtImplies (Greater (Const 1) (Const 2)) (Greater (Const 3) (Const 4)))
      (smtImplies (Greater (Const 1) (Const 2)) (Greater (Const 3) (Const 4)))
  , hashNeqCase "Implies/payload"
      (smtImplies (Greater (Const 1) (Const 2)) (Greater (Const 3) (Const 4)))
      (smtImplies (Greater (Const 3) (Const 4)) (Greater (Const 1) (Const 2)))
  , hashEqCase "Var/equal"
      (toSMT (Op "x"))
      (toSMT (Op "x"))
  , hashNeqCase "Var/payload"
      (toSMT (Op "x"))
      (toSMT (Op "y"))
  , hashNeqCase "Add/Var"
      (smtAdd [Const 1, Const 2])
      (toSMT (Op "x"))
  , hashEqCase "Const/equal"
      (Const 1)
      (Const 1)
  , hashNeqCase "Const/payload"
      (Const 1)
      (Const 2)
  , hashNeqCase "Var/Const"
      (toSMT (Op "x"))
      (Const 1)
  ]

tests :: TestTree
tests = testGroup "SMTExpr instances"
  [ eqTests
  , ordTests
  , hashTests
  ]
