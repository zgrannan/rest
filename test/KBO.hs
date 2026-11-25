{-# LANGUAGE OverloadedStrings #-}

module KBO where

import Language.REST.KBO
import Language.REST.SMT
import Language.REST.RuntimeTerm
import Nat () -- IsString RuntimeTerm

tests :: [(String, IO Bool)]
tests = testList where

  gte :: RuntimeTerm ->  RuntimeTerm -> IO Bool
  gte t u = withZ3 $ \hs ->
    checkSat' hs (kboGTE t u)

  testList =
    [ ("Comm"  , gte "f(a, b)" "f(b, a)")
    , ("Assoc" , gte "f(a, f(b, c))"  "f((a b), c)")
    , ("Dist"  , not <$> gte "f(a, g(b, c))" "g(f(a, b), f(a, c))")
    ]
