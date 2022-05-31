{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.REST.Internal.WorkStrategy where

import Language.REST.ExploredTerms as ET
import Language.REST.Path
import Language.REST.Internal.Rewrite

import Data.Hashable
import qualified Data.List as L

type GetWork m rule term oc = [Path rule term oc] -> ExploredTerms term oc m -> (Path rule term oc, [Path rule term oc])

newtype WorkStrategy rule term oc = WorkStrategy (forall m . GetWork m rule term oc)

bfs :: WorkStrategy rule term oc
bfs = WorkStrategy bfs'

notVisitedFirst :: (Eq term, Eq rule, Eq oc, Hashable term) => WorkStrategy rule term oc
notVisitedFirst = WorkStrategy notVisitedFirst'

bfs' :: [Path rule term oc] ->  ExploredTerms et oc m -> (Path rule term oc, [Path rule term oc])
bfs' (h:t) _ = (h, t)
bfs' _ _ = error "empty path list"

notVisitedFirst' :: (Eq term, Eq rule, Eq oc, Hashable term) => GetWork m rule term oc
notVisitedFirst' paths et =
  case L.find (\p -> not (ET.visited (runtimeTerm p) et)) paths of
    Just p  -> (p, L.delete p paths)
    Nothing -> (head paths, tail paths)

commutesLast :: forall term oc . (Eq term, Eq oc, Hashable term) => WorkStrategy Rewrite term oc
commutesLast = WorkStrategy go where
  go paths et =
    case L.find (\p -> not (ET.visited (runtimeTerm p) et || fromComm p)) paths of
        Just p  -> (p, L.delete p paths)
        Nothing -> (head paths, tail paths)
  fromComm ([], _)    = False
  fromComm (steps, _) = (getName . rule . last) steps == Just "mpComm"
