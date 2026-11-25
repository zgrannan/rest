module WQO where

import Language.REST.Internal.WQO as WQO
import Data.Maybe (isNothing)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

basicInvalid :: Maybe (WQO Char)
basicInvalid = do
  wqo1 <- WQO.singleton ('A', 'C', QGT)
  WQO.insertMaybe wqo1 ('C', 'A', QGT)

tests :: TestTree
tests =
  testGroup "WQO"
    [
      testCase "RejectInvalid" $
        assertBool "RejectInvalid" $ isNothing basicInvalid
    , testCase "NotStrongerThan" $
        case insert empty ("f", "g", QGT) of
          ValidExtension fg ->
            case insert fg ("y", "z", QGT) of
              ValidExtension fgyz ->
                assertBool "NotStrongerThan" $ fg `notStrongerThan` fgyz
              _ ->
                error "Failed to insert (\"y\", \"z\", QGT)"
          _ ->
            error "Failed to insert (\"f\", \"g\", QGT)"
    ]
