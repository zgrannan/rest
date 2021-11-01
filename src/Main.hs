{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Identity
import Data.Time.Clock
import Data.Hashable
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import qualified Data.Maybe as Mb
import Debug.Trace
import Data.List (intercalate)
import Control.Monad
import Data.Bifunctor (bimap)
import Text.Printf

import qualified Arith as A
import qualified Compiler as C
import qualified Group as G
import BagExample
import WQODot as WQODot
import Language.REST.RESTDot
import Language.REST.Dot
import DSL
import Nat
import qualified Set as Set
import qualified Multiset as MS
import NonTerm as NT
import qualified Lists as Li

import Language.REST.MultisetOrder (possibilities)
import Language.REST.ConcreteOC
import Language.REST.Core
import Language.REST.AbstractOC
import Language.REST.OCToAbstract
import Language.REST.Op
import Language.REST.OpOrdering
import Language.REST.OrderingConstraints as OC
import qualified Language.REST.OrderingConstraints.Strict as SC
import qualified Language.REST.OrderingConstraints.Lazy   as LC
import qualified Language.REST.OrderingConstraints.ADT    as AC
import Language.REST.KBO (kbo)
import Language.REST.RPO
import Language.REST.WQO as WQO
import Language.REST.WorkStrategy
import Language.REST.Path
import Language.REST.ProofGen
import Language.REST.Rest
import Language.REST.Types
import Language.REST.SMT
import qualified Language.REST.MetaTerm as MT
import           Language.REST.RuntimeTerm as RT
import           Language.REST.Rewrite


data ConsType = Strict | Lazy | ADT

mkArithRESTGraph ct = mkRESTGraph ct A.evalRWs A.userRWs
mkCompilerRESTGraph ct = mkRESTGraph ct C.evalRWs C.userRWs
mkGroupRESTGraph ct = mkRESTGraph ct G.evalRWs G.userRWs
mkListsRESTGraph ct = mkRESTGraph ct Li.evalRWs Li.userRWs
mkSetsRESTGraph ct = mkRESTGraph ct Set.evalRWs Set.userRWs
mkNonTermRestGraph ct = mkRESTGraph ct NT.evalRWs NT.userRWs
mkMSRESTGraph ct = mkRESTGraph ct MS.evalRWs MS.userRWs


explain z3 (t, u) = printf "%s ≥ %s requires:\n%s\n\n\n" (show t) (show u) (show $ rpoGTE t u)
  where
    ?impl = AC.adtOC z3

explainOrient :: [String] -> IO ()
explainOrient ts0 = withZ3 go where
  go z3 =
    let
      ts             = map parseTerm ts0
      pairs          = zip ts (tail ts)
    in
      do
        mapM_ (explain z3) pairs
        printf "Result:\n%s\n" (show $ orient ts)
        (isSatisfiable (AC.adtOC z3) (orient ts)) >>= print
    where
      ?impl = lift (AC.adtOC z3) rpo

data GraphParams = GraphParams
  {  gShowConstraints :: Bool
  ,  gTarget          :: Maybe String
  ,  gGraphType        :: GraphType
  }

defaultParams :: GraphParams
defaultParams = GraphParams False Nothing Tree

withTarget :: String -> GraphParams -> GraphParams
withTarget target gp = gp{gTarget = Just target}

withShowConstraints :: GraphParams -> GraphParams
withShowConstraints gp = gp{gShowConstraints = True}

data SolverType = RPO | KBO | Fuel Int

mkRESTGraph ::
     SolverType
  -> S.HashSet Rewrite
  -> S.HashSet Rewrite
  -> String
  -> String
  -> GraphParams
  -> IO ()
mkRESTGraph RPO evalRWs userRWs name term params =
  withZ3 $ \z3 -> mkRESTGraph' (lift (AC.adtOC z3) rpo) evalRWs userRWs name term params
mkRESTGraph KBO evalRWs userRWs name term params =
  withZ3 $ \z3 -> mkRESTGraph' (kbo z3) evalRWs userRWs name term params
mkRESTGraph (Fuel n) evalRWs userRWs name term params =
  mkRESTGraph' (fuelOC n) evalRWs userRWs name term params

mkRESTGraph' :: (MonadIO m, Show c, Hashable c, Ord c) =>
     AbstractOC c RuntimeTerm m
  -> S.HashSet Rewrite
  -> S.HashSet Rewrite
  -> String
  -> String
  -> GraphParams
  -> m ()
mkRESTGraph' impl evalRWs userRWs name term params =
  do
    let pr (Rewrite t u _) = printf "%s → %s" (pp t) (pp u)
    liftIO $ mapM_ (\rw -> putStrLn $ pr rw) $ S.toList userRWs
    liftIO $ mapM_ (\rw -> putStrLn $ pr rw) $ S.toList evalRWs
    start <- liftIO $ getCurrentTime
    (PathsResult paths, targetPath) <- rest
      RESTParams
        { re           = evalRWs
        , ru           = userRWs
        , toET         = id
        , target       = fmap parseTerm (gTarget params)
        , workStrategy = notVisitedFirst
        , ocImpl       = impl
        , initRes      = pathsResult
        } (parseTerm term)
    end <- liftIO $ getCurrentTime
    liftIO $ printf "REST run completed, in %s\n" $ show $ diffUTCTime end start
    liftIO $ putStrLn "Drawing graph"
    let showCons = if gShowConstraints params then show else const ""
    let prettyPrinter = PrettyPrinter pr pp showCons True
    liftIO $ writeDot name (gGraphType params) prettyPrinter (toOrderedSet paths)
    liftIO $ case gTarget params of
      Just target ->
        (case targetPath of
          Just tp -> printf "FOUND TARGET. Proof:\n%s\n" (toProof tp)
          Nothing -> printf "TARGET %s NOT FOUND\n" (pp (parseTerm target)))
      Nothing -> return ()



lhs :: RuntimeTerm
lhs = "ite(isNull(ys), null, reverse(tail(ys)) + cons(head(ys),null)) + (reverse(tail(ds)) + cons(head(ds), null))"

rhs :: RuntimeTerm
rhs = "(ite(isNull(ys), null, reverse(tail(ys)) + cons(head(ys), null)) + reverse(tail(ds))) + cons(head(ds), null)"

mkWQOGraph :: String -> String -> IO ()
mkWQOGraph name wqoS = mkWQOGraph' name wqo where
  Just wqo = parseOO wqoS

mkWQOGraph' :: (Ord a, Hashable a, Show a) => String -> WQO.WQO a -> IO ()
mkWQOGraph' name wqo = mkGraph name (WQODot.toDigraph wqo)

main :: IO ()
main = return ()
