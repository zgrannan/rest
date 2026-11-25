{-# LANGUAGE OverloadedStrings #-}
module StrictOC where

import Data.Maybe

import Language.REST.WQOConstraints.Strict
import Language.REST.Internal.WQO
import Language.REST.Internal.OpOrdering

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

tests :: TestTree
tests = testGroup "StrictOC"
  [ testCase "permits" $ assertBool "permits" (permits noConstraints wqo)
  , testCase "permits2" $ assertBool "permits2" $
      permits noConstraints $ fromJust $ parseOO "+ = f = nil = s ∧ + > g ∧ + > h ∧ cons > + ∧ cons > g"
  ] where
  wqo = fromJust $ mergeAll
                      [ "cons" =. "z"
                      , "g" =. "nil"
                      , "h" =. "s"
                      , "cons" >. "g"
                      , "cons" >. "h"
                      , "h" >. "f"
                      , "h" >. "g"
                      ]
