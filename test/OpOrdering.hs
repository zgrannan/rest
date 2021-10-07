{-# LANGUAGE OverloadedStrings #-}

module OpOrdering where

import Language.REST.Op
import Language.REST.OpOrdering
import Language.REST.WQO
import Data.Maybe as Mb

tests = [
  ("parse",
     fromJust (
      parseOO "cons = z ∧ g = nil ∧ h = s ∧ cons > g ∧ cons > h ∧ h > f ∧ h > g"
     ) == wqo)
  ,("minimal",
    fromJust (
      parseOO "f > + ^ g > z ^ + = cons ^ cons > z ^ f = g"
    ) ==
    fromJust (
      parseOO "f > + ^ + = cons ^ cons > z ^ f = g"
    )
  ) ]
        where

        Just wqo = mergeAll [ ("cons" =. "z")
                , ("g" =. "nil")
                , ("h" =. "s")
                , ("cons" >. "g")
                , ("cons" >. "h")
                , ("h" >. "f")
                , ("h" >. "g")
                ]
