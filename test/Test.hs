{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Arith as A

import Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified ExploredTerms
import OpOrdering
import DSL
import WQO
import MultisetOrder
import Nat
import RPO
import RPOEquiv
import LPOEquiv
import KBO
import StrictOC
import SMT
import qualified SMTExprInstances
import qualified QuickCheckTests

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, assertBool)
import Control.Exception (bracket, finally)

import Language.REST (lpoOCA, rpoOCA)
import Language.REST.ExploredTerms
import Language.REST.OCAlgebra
import Language.REST.Core
import Language.REST.KBO (kbo)
import Language.REST.WQOConstraints as OC
import Language.REST.WQOConstraints.SMT (smtOC)
import qualified Language.REST.WQOConstraints.Strict as SC (strictOC)
import Language.REST.Op
import Language.REST.RPO
import Language.REST.Internal.OpOrdering
import Language.REST.RewriteRule
import Language.REST.RuntimeTerm
import Language.REST.MetaTerm as MT
import Language.REST.Internal.Rewrite
import Language.REST.Internal.WQO
import Language.REST.Rest
import Language.REST.SMT
import Language.REST.Internal.WorkStrategy
import qualified Data.Maybe as Mb
import qualified Data.HashSet as S


diverges :: (Show oc) => OCAlgebra oc RuntimeTerm IO -> [RuntimeTerm] -> IO Bool
diverges impl ts = not <$> (orient impl ts >>= isSat impl)

rewrites :: (Show oc, Hashable oc, Eq oc)
  => OCAlgebra oc RuntimeTerm IO
  -> S.HashSet Rewrite -> S.HashSet Rewrite -> RuntimeTerm -> IO (S.HashSet RuntimeTerm)
rewrites impl evalRWs userRWs t0 =
  resultTerms . fst <$> rest
    RESTParams
      { re           = evalRWs
      , ru           = userRWs
      , target       = Nothing
      , workStrategy = notVisitedFirst
      , ocImpl       = ?impl
      , initRes      = termsResult
      , etStrategy   = ExploreWhenNeeded
      } t0
  where
    ?impl = impl

ocaGroup :: String
         -> OCAlgebra (SMTExpr Bool) RuntimeTerm IO
         -> [String]
         -> TestTree
ocaGroup name impl toSkip =
  testGroup name
    [ testGroup "Arith"
        [ testCase n (t >>= assertBool n)
        | (n, t) <- arithTests impl
        , n `notElem` toSkip
        ]
    , testGroup "Complete"
        [ testCase n (t >>= assertBool n)
        | (n, t) <- completeTests impl
        , n `notElem` toSkip
        ]
    ]


orderingTests :: TestTree
orderingTests =
  let ?impl = SC.strictOC @IO in
  let permits' = permits ?impl in
    testGroup "Ordering"
      [
        testCase "simple1" $
          assertBool "simple1" $ not $ rpoGTE "f(t1)" "g(t2)" `permits'` (t1Op =. t2Op)
      , testCase "simple2" $
          assertBool "simple2" $ rpoGTE "f(t1)" "g(t2)" `permits'` Mb.fromJust (merge (f >. g) (t1Op =. t2Op))
      , testCase "simple3" $
          assertBool "simple3" $ rpoGTE "f(t1)" "g(t2)" `permits'` Mb.fromJust (merge (f >. g) (t1Op >. t2Op))
      , testCase "subterm" $
          assertBool "subterm" $
            rpoGTE "f(g)" "f" == noConstraints ?impl
      , testCase "intersect" $ do
          isUnsat <- OC.isUnsatisfiable ?impl $ OC.intersect ?impl (OC.singleton ?impl (f  >. g)) (OC.singleton ?impl (g >. f))
          assertBool "intersect" isUnsat
      ]

proveEQ :: (Show oc, Hashable oc, Eq oc) =>
     OCAlgebra oc RuntimeTerm IO
  -> S.HashSet Rewrite -> S.HashSet Rewrite
  -> RuntimeTerm -> RuntimeTerm -> IO Bool
proveEQ impl evalRWs userRWs have want =
  do
    rw1 <- rewrites impl evalRWs userRWs have
    rw2 <- rewrites impl evalRWs userRWs want
    return $ not $ disjoint rw1 rw2
  where
    disjoint s1 s2 = S.null $ s1 `S.intersection` s2

eval :: S.HashSet Rewrite -> RuntimeTerm -> IO RuntimeTerm
eval rws t0 =
  do
    result <- mapM (apply t0) (S.toList rws)
    case S.toList $ S.unions result of
      []      -> return t0
      (t : _) -> eval rws t

arithTests :: (Show oc, Hashable oc, Eq oc) => OCAlgebra oc RuntimeTerm IO -> [(String, IO Bool)]
arithTests impl =
  [
    ("Contains", return $ contains (intToTerm 2) (intToTerm 1))
  , ("Diverge", not <$> diverges impl [ intToTerm 2 .+ t1
                               , intToTerm 1 .+ t1
                               ])
  , ("Diverge3", not <$> diverges impl [ (t1 .+ t2) .+ t3
                               , t1 .+ (t2 .+ t3)
                               , (t2 .+ t3) .+ t1
                               ])
  , ("Eval1", arithEQ (intToTerm 2 .+ intToTerm 3) 5)
  , ("Eval2", arithEQ (ack (intToTerm 3) (intToTerm 2)) 29)
  , ("Subst1", return $ subst (M.fromList [("X", intToTerm 1), ("Y", intToTerm 2)]) (x #+ y) == (intToTerm 1 .+ intToTerm 2))
  , ("ArithTerm", termTest)
  , ("ArithTerm2", termTest2)
  , ("Arith0", eq (t1 .+ t2 .+ intToTerm 1) (t1 .+ (intToTerm 1 .+ t2)))
  , ("Arith1", eq (intToTerm 2 .+ intToTerm 3) (intToTerm 3 .+ intToTerm 2))
  , ("Arith2", eq (t1 .+ t2) (t2 .+ t1))
  , ("Arith3", not <$> eq (t2 .+ t1) (t2 .+ t2))
  , ("Arith4", eq ((t1 .+ t2) .+ t3) (t1 .+ (t2.+ t3)))
  , ("Arith4.1", eq (t1 .+ t2 .+ t3) (t3 .+ t2 .+ t1))
  , ("Arith5", eq (zero .+ t1) t1)
  , ("Arith5.1", eq (zero .+ zero .+ t1) t1)
  , ("Arith5.2", eq (zero .+ zero .+ zero .+ t1) t1)
  , ("Arith6", eq (((t1 .+ t1) .+ t3) .+ t4) (t1 .+ (t1 .+ (t3 .+ t4))))
  , ("Arith7", eq ((intToTerm 2 .+ intToTerm 1) .+ t1) (intToTerm 2 .+ (intToTerm 1 .+ t1)))
  ]
  where

    arithEQ t n = do
      t' <- eval A.evalRWs t
      return $ termToInt t' == Just n


    termTest = proveEQ impl evalRWs userRWs (App f1 [t1]) zero
      where
        evalRWs = S.union termEvalRWs  A.evalRWs
        userRWs = S.insert (MT.RWApp g1 [x] ~> MT.RWApp f1 [x]) A.userRWs
        termEvalRWs = S.fromList
          [  MT.RWApp f1 [x] ~> MT.RWApp g1 [suc' x]
          ,  MT.RWApp g1 [x] ~> zero'
          ]
        f1 = Op "f"
        g1 = Op "g"

    termTest2 = proveEQ impl evalRWs userRWs (App f1 [zero]) (App g1 [zero])
      where
        evalRWs = S.union termEvalRWs A.evalRWs
        userRWs = S.insert (MT.RWApp f1 [x] ~> MT.RWApp g1 [suc' (suc' x)]) A.userRWs
        termEvalRWs = S.fromList
          [  MT.RWApp f1 [suc' x] ~> MT.RWApp g1 [suc' x]
          ,  MT.RWApp f1 [zero']  ~> zero'
          ,  MT.RWApp g1 [suc' x] ~> MT.RWApp f1 [x]
          ,  MT.RWApp g1 [zero']  ~> zero'
          ]
        f1 = Op "f"
        g1 = Op "g"


    eq = proveEQ impl A.evalRWs A.userRWs

completeTests :: (Show oc, Hashable oc, Eq oc) => OCAlgebra oc RuntimeTerm IO -> [(String, IO Bool)]
completeTests impl =
  [ ("CompleteDiverges", not <$> diverges impl [App start [], App mid [], App finish []])
  , ("Complete1"     , eq (App start []) (App finish []))
  , ("EvalComplete2" , (== App finish []) <$> eval completeUserRWs (App start' [App s1 []]) )
  , ("Complete2"     , eq (App start' [App s1 []]) (App finish []))
  ]
  where

    completeUserRWs = S.fromList
      [ MT.RWApp start [] ~> MT.RWApp mid []
      , MT.RWApp mid []   ~> MT.RWApp finish []

      , MT.RWApp start' [MT.RWApp s2 []] ~> MT.RWApp mid' [MT.RWApp s1 []]
      , MT.RWApp s1 []                 ~> MT.RWApp s2 []
      , MT.RWApp mid' [MT.RWApp s2 []]   ~> MT.RWApp finish []
      ]

    eq :: RuntimeTerm -> RuntimeTerm -> IO Bool
    eq = proveEQ impl S.empty completeUserRWs

    start  = Op "start"
    mid    = Op "mid"
    finish = Op "finish"

    start'  = Op "start'"
    mid'    = Op "mid'"

    s1      = Op "s1"
    s2      = Op "s2"

ocTests :: SolverHandle -> TestTree
ocTests z3 =
  testGroup "OC tests"
    [ RPO.tests
    , let ?impl = smtOC z3 in RPO.solverTests
    , orderingTests
    ]

main :: IO ()
main = withZ3 $ \z3 ->
  withZ3' $ \kboZ3 ->
  withZ3' $ \rpoZ3 ->
  withZ3' $ \lpoZ3 ->  do
    kboImpl <- kbo kboZ3 "$KBO_"
    rpoImpl <- rpoOCA rpoZ3 "$R_"
    lpoImpl <- lpoOCA lpoZ3 "$L_"
    flip finally (mapM_ killZ3 [z3, kboZ3, rpoZ3, lpoZ3]) $
      defaultMain $ testGroup "REST"
        [ ExploredTerms.tests
        , SMT.tests
        , SMTExprInstances.tests
        , KBO.tests
        , QuickCheckTests.tests
        , OpOrdering.tests
        , StrictOC.tests
        , ocTests z3
        , RPOEquiv.tests z3
        , LPOEquiv.tests z3
        , MultisetOrder.tests
        , WQO.tests
        , ocaGroup "KBO" kboImpl []
        , ocaGroup "RPO" rpoImpl []
        , ocaGroup "LPO" lpoImpl ["Diverge3", "Arith4", "Arith4.1", "Arith6"]
        ]
  where
    withZ3' :: (SolverHandle -> IO a) -> IO a
    withZ3' = bracket spawnZ3 killZ3
