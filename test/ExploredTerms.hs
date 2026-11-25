{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ExploredTerms where

import Control.Monad.Identity
import Data.Hashable
import qualified Data.HashSet as S
import GHC.Generics (Generic)

import Language.REST.ExploredTerms as ET

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

type Constraints = Int

-- 2nd argument is cost to explore
data Term = Term String Int
  deriving (Eq, Generic, Hashable, Show)

exploreFuncs :: ExploreFuncs Term Constraints Identity
exploreFuncs = EF undefined subsume refine where
  subsume c0 c1               = return $ c0 >= c1
  refine  c  _  (Term _ dest) = return $ c - dest

t0 :: Term
t0 = Term "t0" 5

t1 :: Term
t1 = Term "t1" 0

t2 :: Term
t2 = Term "t2" 0

et0 :: ExploredTerms Term Constraints Identity
et0 = ET.empty exploreFuncs ExploreWhenNeeded

et :: ExploredTerms Term Constraints Identity
et = ET.insert t1 15 (S.fromList [t0]) $ ET.insert t0 14 (S.fromList [t2]) et0

tests :: TestTree
tests = testGroup "ExploredTerms"
  [ -- Described in https://github.com/zgrannan/rest/issues/9
    testCase "Explore-opt" $ assertBool "Explore-opt" $ not $ runIdentity $ shouldExplore t1 17 et
  , testCase "Explore" $ assertBool "Explore" $ runIdentity $ shouldExplore t1 21 et
  ]
