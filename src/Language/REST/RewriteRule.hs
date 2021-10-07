{-# LANGUAGE MultiParamTypeClasses #-}

module Language.REST.RewriteRule where

import qualified Data.HashSet as S

class RewriteRule m rule term where
  apply :: term -> rule -> m (S.HashSet term)
