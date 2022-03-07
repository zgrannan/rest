{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module implements the optimizations to prune the
-- exploration of rewrites of terms that have been already considered
-- (section 6.4 of the REST paper).
module Language.REST.ExploredTerms
   (
     ExploredTerms
   , empty
   , insert
   , shouldExplore
   , size
   , visited
   , ExploreFuncs(..)
   , ExploreStrategy(..)
   )  where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Hashable

import Prelude hiding (lookup)

data ExploreStrategy =
  ExploreAlways | ExploreLessConstrained | ExploreWhenNeeded | ExploreOnce

data ExploreFuncs c m = EF
  { union           :: c -> c -> c
    -- | @c0 `subsumes` c1@ if @c0@ permits all orderings permited by @c1@
  , subsumes        :: c -> c -> m Bool
  }

-- A mapping of terms, to the rewritten terms that need to be fully explored
-- in order for this term to be fully explored
data ExploredTerms term c m =
  ET (M.HashMap term (c, (S.HashSet term))) (ExploreFuncs c m) ExploreStrategy

trace' :: String -> b -> b
-- trace' = trace
trace' _ x = x


size :: ExploredTerms term c m -> Int
size (ET m _ _) = M.size m

empty :: ExploreFuncs c m -> ExploreStrategy -> ExploredTerms term c m
empty = ET M.empty

visited :: (Eq term, Hashable term) => term -> ExploredTerms term c m -> Bool
visited t (ET m _ _) = M.member t m

insert :: (Eq term, Hashable term) => term -> c -> S.HashSet term -> ExploredTerms term c m -> ExploredTerms term c m
insert t oc s (ET etMap ef@(EF union _ ) strategy) = ET (M.insertWith go t (oc, s) etMap) ef strategy
  where
    go (oc1, s1) (oc2, s2) = (union oc1 oc2, S.union s1 s2)

lookup :: (Eq term, Hashable term) => term -> ExploredTerms term c m -> Maybe (c, (S.HashSet term))
lookup t (ET etMap _ _) = M.lookup t etMap

-- | @isFullyExplored t c M = not explorable(t, c)@ where @explorable@ is
-- defined as in the REST paper.
isFullyExplored :: forall term c m . (Monad m, Show term, Eq term, Hashable term, Eq c) =>
  term -> c -> ExploredTerms term c m -> m Bool
isFullyExplored t0 oc0 et@(ET _ (EF{subsumes}) _) = result where

  result = go S.empty [t0]
    -- if (trace ("Check " ++ show t0) go) S.empty [t0]
    -- then trace (show t0 ++ " is fully explored.") True
    -- else False

  go :: S.HashSet term -> [term] -> m Bool
  go _ []       = return True
  go seen (h:t) | Just (oc, trms) <- lookup h et
                = do
                    haveExploredAllCurrentlyPermitedOrderings <- oc `subsumes` oc0
                    if haveExploredAllCurrentlyPermitedOrderings
                      then go seen' t
                      else
                        let ts = (S.union trms (S.fromList t)) `S.difference` seen'
                        in go seen' (S.toList ts)
                  where
                    seen' = S.insert h seen

  go _ _        | otherwise = trace' "GF" $ return False -- trace ("Must check " ++ show t0 ++ " . Visited: " ++ (show $ visited t0 et)) False

shouldExplore :: forall term c m . (Monad m, Show term, Eq term, Hashable term, Eq c, Show c) =>
  term -> c -> ExploredTerms term c m -> m Bool
shouldExplore t oc et@(ET _ EF{subsumes} strategy) =
  case strategy of
    ExploreWhenNeeded      -> not <$> isFullyExplored t oc et
    ExploreOnce            -> return $ not $ visited t et
    ExploreAlways          -> return True
    ExploreLessConstrained ->
      case lookup t et of
        Just (oc', _) -> do
          s <- oc' `subsumes` oc
          return  $ if s
            then trace' ((show oc') ++ " subsumes " ++ (show oc)) False
            else True
        Nothing       -> return True
