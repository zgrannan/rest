{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.REST.WorkStrategy where

import Language.REST.ExploredTerms as ET
import Language.REST.Path
import Language.REST.Rewrite

import Data.Hashable
import qualified Data.List as L

type GetWork m rule term et oc = [Path rule term oc] -> (term -> et) -> ExploredTerms et oc m -> (Path rule term oc, [Path rule term oc])

newtype WorkStrategy rule term et oc = WorkStrategy (forall m . GetWork m rule term et oc)

bfs = WorkStrategy bfs'

notVisitedFirst :: (Eq term, Eq rule, Eq oc, Eq et, Hashable et) => WorkStrategy rule term et oc
notVisitedFirst = WorkStrategy notVisitedFirst'

bfs' :: [Path rule term oc] -> (term -> et) -> ExploredTerms et oc m -> (Path rule term oc, [Path rule term oc])
bfs' (h:t) _ _ = (h, t)

notVisitedFirst' :: (Eq term, Eq rule, Eq oc, Eq et, Hashable et) => GetWork m rule term et oc
notVisitedFirst' paths toET et =
  case L.find (\p -> not (ET.visited (toET $ runtimeTerm p) et)) paths of
    Just p  -> (p, L.delete p paths)
    Nothing -> (head paths, tail paths)

commutesLast :: forall term oc et . (Eq term, Eq oc, Eq et, Hashable et) => WorkStrategy Rewrite term et oc
commutesLast = WorkStrategy go where
  go paths toET et =
    case L.find (\p -> not (ET.visited (toET $ runtimeTerm p) et || fromComm p)) paths of
        Just p  -> (p, L.delete p paths)
        Nothing -> (head paths, tail paths)
  fromComm ([], _)    = False
  fromComm (steps, _) = (getName . rule . last) steps == Just "mpComm"
