{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.REST.Path where

import qualified Data.HashSet as S
import GHC.Generics (Generic)
import Data.Hashable
import Language.REST.Types

data Step rule term a = Step {
    term     :: PathTerm rule term
  , rule     :: rule
  , ordering :: a
  , fromPLE  :: Bool
} deriving (Eq, Ord, Generic, Hashable)


data PathTerm rule term = PathTerm
    {  pathTerm :: term

       -- The orderings FROM pathTerm that were rejected
    ,  rejected :: S.HashSet (term, rule)
    } deriving (Eq, Ord, Generic, Hashable)

type Path rule term a = ([Step rule term a], PathTerm rule term)

pathTerms :: Path rule term a -> [term]
pathTerms (xs, x) = map pathTerm $ map term xs ++ [x]

runtimeTerm :: Path rule term a -> term
runtimeTerm (_, pt) = pathTerm pt
