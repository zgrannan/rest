module WQO where

import Language.REST.Internal.WQO as WQO
import Data.Maybe (isNothing)

basicInvalid :: Maybe (WQO Char)
basicInvalid = do
  wqo1 <- WQO.singleton ('A', 'C', QGT)
  WQO.insertMaybe wqo1 ('C', 'A', QGT)

tests :: [(String, Bool)]
tests =
  case insert empty ("f", "g", QGT) of
    ValidExtension fg ->
      case insert fg ("y", "z", QGT) of
        ValidExtension fgyz ->
          [ ("NotStrongerThan", fg `notStrongerThan` fgyz)
          , ("RejectInvalid", isNothing basicInvalid)
          ]
        _ -> error "Failed to insert (\"y\", \"z\", QGT)"
    _ -> error "Failed to insert (\"f\", \"g\", QGT)"
