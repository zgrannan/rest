{-# LANGUAGE OverloadedStrings #-}
module StrictOC where

import Data.Maybe

import Language.REST.WQOConstraints.Strict
import Language.REST.Internal.WQO
import Language.REST.Op
import DSL
import Language.REST.Internal.OpOrdering

tests =
  [
    ("permits", permits noConstraints wqo)
  , ("permits2", permits noConstraints $ fromJust $ parseOO "+ = f = nil = s ∧ + > g ∧ + > h ∧ cons > + ∧ cons > g")
  ] where
  Just wqo = mergeAll [ ("cons" =. "z")
                      , ("g" =. "nil")
                      , ("h" =. "s")
                      , ("cons" >. "g")
                      , ("cons" >. "h")
                      , ("h" >. "f")
                      , ("h" >. "g")
                      ]
