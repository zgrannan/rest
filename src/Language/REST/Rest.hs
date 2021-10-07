{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.REST.Rest (
    rest
  , pathsResult
  , termsResult
  , terms
  , PathsResult(..)
  , WorkStrategy(..)
  , RESTParams(..)
  ) where

import           Control.Monad
import           Control.Monad.List
import Data.Hashable
import qualified Data.HashSet as S
import qualified Data.List    as L
import qualified Data.HashMap.Strict     as M
import qualified Data.Maybe   as Mb
import Debug.Trace
import Text.Printf

import Language.REST.AbstractOC as AC
import Language.REST.RewriteRule
import Language.REST.Path
import Language.REST.Types
import Language.REST.ExploredTerms as ET
import Language.REST.WorkStrategy

newtype PathsResult rule term oc = PathsResult (S.HashSet (Path rule term oc))

newtype TermsResult rule term oc = TermsResult (S.HashSet term)

pathsResult = PathsResult S.empty
termsResult = TermsResult S.empty

class RESTResult a where
  includeInResult :: (Hashable oc, Eq oc, Hashable rule, Eq rule, Hashable term, Eq term) => Path rule term oc -> a rule term oc -> a rule term oc
  terms :: (Eq term, Hashable term) => a rule term oc -> S.HashSet term

instance RESTResult PathsResult where
  includeInResult p (PathsResult s) = PathsResult (S.insert p s)
  terms (PathsResult s) = S.fromList (concatMap pathTerms $ S.toList s)


instance RESTResult TermsResult where
  includeInResult p (TermsResult s) = TermsResult (S.union s (S.fromList $ pathTerms p))
  terms (TermsResult s)             = s


data RESTState m rule term oc et rtype = RESTState
  { finished   :: rtype rule term oc
  , working    :: [Path rule term oc]
  , explored   :: ExploredTerms et oc m
  , targetPath :: Maybe (Path rule term oc)
  }

data RESTParams m rule term oc et rtype = RESTParams
  { re           :: S.HashSet rule
  , ru           :: S.HashSet rule
  , toET         :: term -> et
  , target       :: Maybe term
  , workStrategy :: WorkStrategy rule term et oc
  , ocImpl       :: AbstractOC oc term m
  , initRes      :: rtype rule term oc
  }

rest :: forall m rule term oc et rtype .
  ( MonadIO m
  , RewriteRule m rule term
  , Show et
  , Hashable term
  , Eq term
  , Hashable rule
  , Hashable et
  , Hashable oc
  , Eq rule
  , Eq et
  , Eq oc
  , Show oc
  , RESTResult rtype)
  => RESTParams m rule term oc et rtype
  -> term
  -> m ((rtype rule term oc), Maybe (Path rule term oc))
rest RESTParams{re,ru,toET,ocImpl,workStrategy,initRes,target} t =
  rest' (RESTState initRes [([], PathTerm t S.empty)] initET Nothing)
  where
    (WorkStrategy ws) = workStrategy
    initET = ET.empty $ EF (AC.union ocImpl) (AC.notStrongerThan ocImpl)

    rest' (RESTState fin [] _ targetPath)            = return (fin, targetPath)
    rest' state@(RESTState _   paths et (Just targetPath))
      | ((steps, _), remaining) <- ws paths toET et
      , length steps >= length (fst targetPath)
      = rest' state{working = remaining}
    rest' state@(RESTState fin paths et targetPath) = do
      se <- shouldExplore (toET t) lastOrdering et
      if se
        then do
          evalRWs <- candidates re -- trace ("Explore " ++ (show $ toET t)) $ candidates re
          userRWs <- candidates ru
          acceptedUserRWs <- accepted userRWs
          go evalRWs userRWs acceptedUserRWs
        else
          rest' (state{ working = remaining })
      where

        (path@(ts, PathTerm t _), remaining) = ws paths toET et

        lastOrdering :: oc
        lastOrdering = if L.null ts then top ocImpl else ordering $ last ts

        tsTerms :: [term]
        tsTerms = pathTerms path

        liftSet :: S.HashSet a -> ListT m a
        liftSet s = ListT $ return $ S.toList s

        candidates :: S.HashSet rule -> m (S.HashSet (term, rule))
        candidates rules = fmap S.fromList res
          where
            res :: m [(term, rule)]
            res = runListT $ do
              r   <- liftSet rules
              t'  <- ListT $ S.toList <$> apply t r
              return (t', r)

        accepted :: (S.HashSet (term, rule)) -> m (M.HashMap term oc)
        accepted userRWs = M.fromList <$> (runListT $ do
          t' <- liftSet $ S.map fst userRWs
          guard $ L.notElem t' tsTerms
          let ord = refine ocImpl lastOrdering t t'
          ok <- lift $ isSat ocImpl ord
          guard ok
          return (t', ord))

        go evalRWs userRWs acceptedUserRewrites =
          do
            ep <- evalPaths
            up <- userPaths
            rest' (state' (ep ++ up))
          where

            state' p' = state
              { working  = p' ++ remaining
              , finished = if null p' then includeInResult (ts, pt) fin else fin
              , explored =
                  let
                    deps = S.map (toET . fst) (S.union evalRWs userRWs)
                  in
                    ET.insert (toET t) lastOrdering deps et
              , targetPath =
                if Just t == target then
                  case targetPath of
                    Just (tp, _) | length tp <= length ts -> targetPath
                    _                                     -> Just (ts, pt)
                else
                  targetPath
              }


            pt = PathTerm t rejectedUserRewrites

            rejectedUserRewrites :: S.HashSet (term, rule)
            rejectedUserRewrites = S.fromList $ do
              (t', r) <- S.toList userRWs
              guard $ L.notElem t' tsTerms
              guard $ Mb.isNothing $ M.lookup t' acceptedUserRewrites
              return (t', r)


            evalPaths = runListT $ do
              (t', r) <- ListT $ return (S.toList evalRWs)
              guard $ L.notElem t' tsTerms
              let ord = refine ocImpl lastOrdering t t'
              lift (shouldExplore (toET t') ord et) >>= guard
              return (ts ++ [Step pt r ord True], PathTerm t' S.empty)

            userPaths = runListT $ do
              (t', r) <- liftSet userRWs
              ord <- ListT $ return $ Mb.maybeToList $ M.lookup t' acceptedUserRewrites
              lift (shouldExplore (toET t') ord et) >>= guard
              return (ts ++ [Step pt r ord False], PathTerm t' S.empty)
