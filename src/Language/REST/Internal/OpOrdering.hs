{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Language.REST.Internal.OpOrdering (
    empty
  , merge
  , OpOrdering
  , opInsert
  , opGT
  , opEQ
  , (=.)
  , (>.)
  , parseOO
  ) where

import Prelude hiding (GT, EQ)
import Data.Maybe
import qualified Data.Text as T
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec
import Text.Parsec (Parsec)

import           Language.REST.Op
import           Language.REST.Internal.WQO as WQO


type OpOrdering   = WQO Op


opGT :: OpOrdering -> Op -> Op -> Bool
opGT s f g = getRelation s f g == Just QGT

opEQ :: OpOrdering -> Op -> Op -> Bool
opEQ s f g = getRelation s f g == Just QEQ


opInsert :: OpOrdering -> Op -> Op -> QORelation -> Maybe OpOrdering
opInsert o f g r =
  case WQO.insert o (f, g, r) of
    ValidExtension o' -> Just o'
    _                 -> Nothing

-- The following only are valid if f /= g.

-- precondition : f /= g
(>.) :: Op -> Op -> OpOrdering
(>.) f g = fromJust $ WQO.singleton (f, g, QGT)

-- precondition : f /= g
(=.) :: Op -> Op -> OpOrdering
(=.) f g = fromJust $ WQO.singleton (f, g, QEQ)

-- precondition : f /= g
(<.) :: Op -> Op -> OpOrdering
(<.) f g = g >. f

parseOO :: String -> Maybe OpOrdering
parseOO str =
  case parse parser "" str of
    Left err -> error (show err)
    Right t  -> t

parser :: Parsec String u (Maybe OpOrdering)
parser = fmap mergeAll' (sepBy1 atom conj) where

  mergeAll' :: [Maybe OpOrdering] -> Maybe OpOrdering
  mergeAll' [x]                     = x
  mergeAll' (Just x : Just x' : xs) =
    do
      x'' <- merge x x'
      mergeAll' (Just x'' : xs)
  mergeAll' _                       = Nothing

  conj = spaces >> (char '\8743' <|> char '^') >> spaces
  eq   = spaces >> char '=' >> spaces
  gt   = spaces >> char '>' >> spaces


  atom = try gtAtom <|> try eqAtom

  eqAtom = fmap allEQ (sepBy1 sym (try eq))
    where
      mkEQ f g = WQO.singleton (f, g, QEQ)
      allEQ syms =
        let
          pairs = zipWith mkEQ syms (tail syms)
        in
          mergeAll' pairs

  gtAtom = do
    left  <- sym
    _     <- gt
    right <- sym
    return $ WQO.singleton (left, right, QGT)

  sym = fmap (Op . T.pack) (many (alphaNum <|> char '+' <|> char '*'))
