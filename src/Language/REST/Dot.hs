{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.REST.Dot where

import GHC.Generics
import Data.Hashable
import Data.List
import qualified Data.Set as S
import Text.Printf
import System.Process

data DiGraph = DiGraph String (S.Set Node) (S.Set Edge);

type NodeID =  String

data GraphType = Tree | Dag | Min deriving (Read)

data Node = Node 
    { nodeID     :: NodeID
    , label      :: String
    , nodeStyle  :: String
    , labelColor :: String
    } deriving (Eq, Ord, Show, Generic, Hashable)

data Edge = Edge
    { from      :: NodeID
    , to        :: NodeID
    , edgeLabel :: String
    , edgeColor :: String
    , subLabel  :: String
    , edgeStyle :: String
    } deriving (Eq, Ord, Show, Generic, Hashable)

type DotPath = [Node]

nodeString :: Node -> String
nodeString (Node id label style color) = 
    printf "\t%s [label=\"%s\"\nstyle=\"%s\"\ncolor=\"%s\"];" id label style color

edgeString :: Edge -> String
edgeString (Edge from to label color subLabel style) = 
    let 
        sub = escape subLabel
        escape xs = concatMap go xs
            where
                go '\\' = "\\"
                go '\n' = "<br />"
                go '>'  = "&gt;"
                go '<'  = "&lt;"
                go o    = [o]
        labelPart =
          if label /= ""
          then printf "<font color =\"red\">%s</font>" (escape label)
          else ""
    in
        printf "\t%s -> %s [label = <%s<br/>%s>\ncolor=\"%s\"\nstyle=\"%s\"];" from to labelPart sub color style

graphString :: DiGraph -> String
graphString (DiGraph name nodes edges) = 
    printf "digraph %s {\n%s\n\n%s\n}" name (nodesString) (edgesString)
    where
        nodesString :: String
        nodesString = intercalate "\n" (map nodeString (S.toList nodes))

        edgesString :: String
        edgesString = intercalate "\n" (map edgeString (S.toList edges))


mkGraph :: String -> DiGraph -> IO ()
mkGraph name graph = do
  let dotfile = printf "graphs/%s.dot" name
  let pngfile = printf "graphs/%s.png" name
  writeFile dotfile (graphString graph)
  result <- readProcessWithExitCode "dot" ["-Tpng", dotfile, "-o", pngfile] ""
  print result

