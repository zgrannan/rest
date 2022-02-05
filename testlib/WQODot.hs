module WQODot where

import Data.Hashable
import qualified Data.Set as S

import Language.REST.Dot
import Language.REST.PartialOrder
import Language.REST.WQO

toDigraph :: (Ord a, Hashable a, Show a) => WQO a -> DiGraph
toDigraph wqo = digraph where

  digraph = DiGraph "wqo" nodes edges

  labelFor ec = 'n' : show (abs $ hash ec)

  nodes = S.map toNode (getECs wqo)
  edges = S.fromList $ map toEdge (toList $ getPO wqo)

  toNode ec         = Node (labelFor ec) (show ec) "solid" "black"
  toEdge (ec1, ec2) = Edge (labelFor ec1) (labelFor ec2) "" "black" "" "solid"
