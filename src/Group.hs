{-# LANGUAGE OverloadedStrings #-}

module Group where

import Data.Text
import DSL
import Language.REST.Op
import Language.REST.MetaTerm

import qualified Data.HashSet as S

neg x = RWApp (Op "neg") [x]

evalRWs = S.empty

userRWs =
    S.fromList
      [
          x #+ zero'    ~> x
        , zero'    #+ x ~> x
        , (neg x) #+ x  ~> zero'
        , (x #+ y) #+ v ~> x #+ (y #+ v)
      ]
