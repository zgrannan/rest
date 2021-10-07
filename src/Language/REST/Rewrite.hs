{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.REST.Rewrite where

import GHC.Generics (Generic)

import           Control.Monad.Identity
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import           Text.Printf

import Language.REST.RewriteRule
import Language.REST.MetaTerm as MT
import Language.REST.RuntimeTerm


data Rewrite = Rewrite MetaTerm MetaTerm (Maybe String)
  deriving (Eq, Ord, Generic, Hashable, Show)

type Subst = M.HashMap String RuntimeTerm

getName (Rewrite t u n) = n
named (Rewrite t u _) n = Rewrite t u (Just n)

subst :: Subst -> MetaTerm -> RuntimeTerm
subst s (MT.Var v)  | Just t <- M.lookup v s = t
                    | otherwise
                    = error $ printf "No value for metavar %s during subst %s" (show v) (show s)
subst s (MT.RWApp op xs) = App op (map (subst s) xs)

unifyAll :: Subst -> [(MetaTerm, RuntimeTerm)] -> Maybe Subst
unifyAll su [] = Just su
unifyAll su ((x, y) : ts)
  | Just s <- unify x y su
  = unifyAll s ts
  | otherwise
  = Nothing

unify :: MetaTerm -> RuntimeTerm -> Subst -> Maybe Subst
unify (MT.Var s) term su | M.lookup s su == Just term
  = Just su
unify (MT.Var s) term su | M.lookup s su == Nothing
  = Just $ M.insert s term su
unify (MT.RWApp o1 xs) (App o2 ys) su | o1 == o2 && length xs == length ys =
  unifyAll su (zip xs ys)
unify _ _ _ = Nothing

instance Monad m => RewriteRule m Rewrite RuntimeTerm where
  apply t (Rewrite left right _) = return $ S.unions $ map go (subTerms t)
    where
      go (t', tf) | Just su <- unify left t' M.empty = S.singleton (tf $ subst su right)
      go _        | otherwise                        = S.empty
