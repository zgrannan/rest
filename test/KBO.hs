{-# LANGUAGE OverloadedStrings #-}

module KBO where

import Language.REST.KBO
import Language.REST.SMT
import Language.REST.RuntimeTerm
import Nat () -- IsString RuntimeTerm

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

tests :: TestTree
tests = testGroup "KBO"
  [ testCase "Comm"  $ gte "f(a, b)" "f(b, a)" >>= assertBool "Comm"
  , testCase "Assoc" $ gte "f(a, f(b, c))"  "f((a b), c)" >>= assertBool "Assoc"
  , testCase "Dist"  $ (not <$> gte "f(a, g(b, c))" "g(f(a, b), f(a, c))") >>= assertBool "Dist"
  ]
  where
    gte :: RuntimeTerm ->  RuntimeTerm -> IO Bool
    gte t u = withZ3 $ \hs ->
      checkSat' hs (kboGTE t u)
