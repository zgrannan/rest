{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.REST.RuntimeTerm where

import           Data.Hashable
import           GHC.Generics (Generic)
import           Text.Printf
import qualified Data.List as L

import           Language.REST.Op

data RuntimeTerm = App Op [RuntimeTerm] deriving (Eq, Ord, Generic, Hashable)

instance Show RuntimeTerm where
  show (App op []) = show op
  show (App op ts) = printf "%s(%s)" (show op) $ L.intercalate ", " (map show ts)

class ToRuntimeTerm a where
  toRuntimeTerm :: a -> RuntimeTerm

instance ToRuntimeTerm Op where
  toRuntimeTerm op = App op []

instance ToRuntimeTerm RuntimeTerm where
  toRuntimeTerm = id

subTerms :: RuntimeTerm -> [(RuntimeTerm, (RuntimeTerm -> RuntimeTerm))]
subTerms t@(App f ts) = (t, id) : concatMap st [0..length ts - 1]
  where
    st :: Int -> [(RuntimeTerm, (RuntimeTerm -> RuntimeTerm))]
    st i =
      let
        t = ts !! i
        go t' =
          App f $ take i ts ++ [t'] ++ drop (i + 1) ts
        go2 (st, toFull) = (st, go . toFull)
      in
        map go2 (subTerms t)
