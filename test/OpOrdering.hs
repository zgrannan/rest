{-# LANGUAGE OverloadedStrings #-}

module OpOrdering where

import Language.REST.Internal.OpOrdering
import Language.REST.Internal.WQO
import Data.Maybe as Mb

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

tests :: TestTree
tests = testGroup "OpOrdering"
  [ testCase "parse" $ assertBool "parse" $
      fromJust (
        parseOO "cons = z ∧ g = nil ∧ h = s ∧ cons > g ∧ cons > h ∧ h > f ∧ h > g"
      ) == wqo
  , testCase "minimal" $ assertBool "minimal" $
      fromJust (
        parseOO "f > + ^ g > z ^ + = cons ^ cons > z ^ f = g"
      ) ==
      fromJust (
        parseOO "f > + ^ + = cons ^ cons > z ^ f = g"
      )
  ]
    where

        wqo = fromJust $ mergeAll [ "cons" =. "z"
                , "g" =. "nil"
                , "h" =. "s"
                , "cons" >. "g"
                , "cons" >. "h"
                , "h" >. "f"
                , "h" >. "g"
                ]
