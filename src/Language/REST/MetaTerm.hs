{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.REST.MetaTerm where

import Data.String
import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.Set as S

import Language.REST.Op
import Language.REST.RuntimeTerm

data MetaTerm =
    Var String
  | RWApp Op [MetaTerm] deriving (Eq, Ord, Show, Generic, Hashable)

instance IsString MetaTerm where
  fromString = Var

class ToMetaTerm a where
  toMetaTerm :: a -> MetaTerm

instance ToMetaTerm MetaTerm where
  toMetaTerm = id

instance ToMetaTerm RuntimeTerm where
  toMetaTerm (App f xs) = RWApp f (map toMetaTerm xs)

termOps :: ToMetaTerm a => a -> S.Set Op
termOps = go . toMetaTerm where
  go :: MetaTerm -> S.Set Op
  go (Var _)         = S.empty
  go (RWApp op trms) = S.insert op (S.unions (map go trms))
