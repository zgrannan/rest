{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.REST.MultiSet
  ( MultiSet
  , delete
  , deleteMany
  , distinctElems
  , empty
  , filter
  , insert
  , member
  , null
  , toList
  , toOccurList
  , singleton
  , fromList
  , toSet
  ) where

import Prelude hiding (null, filter, delete)

import GHC.Generics
import Data.Hashable
import qualified Data.List as L
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

data MultiSet a = MultiSet (M.HashMap a Int) deriving (Eq, Generic, Hashable, Ord)

instance Show a => Show (MultiSet a) where
  show ms = "{" ++ L.intercalate ", " (map show $ toList ms) ++ "}"

delete :: (Hashable a, Eq a) => a -> MultiSet a -> MultiSet a
delete k = deleteMany k 1

deleteMany :: (Hashable a, Eq a) => a -> Int -> MultiSet a -> MultiSet a
deleteMany k v (MultiSet ms) | Just c <- M.lookup k ms
                             , c > v = MultiSet $ M.insert k (c - v) ms
deleteMany k _ (MultiSet ms) | otherwise = MultiSet $ M.delete k ms

distinctElems :: MultiSet a -> [a]
distinctElems (MultiSet ms) = M.keys ms

empty :: MultiSet a
empty = MultiSet M.empty

toOccurList :: MultiSet a -> [(a, Int)]
toOccurList (MultiSet ms) = M.toList ms

filter :: (a -> Bool) -> MultiSet a -> MultiSet a
filter f (MultiSet ms) = MultiSet $ M.filterWithKey f' ms
  where
    f' k _ = f k

null :: MultiSet a -> Bool
null (MultiSet ms) = M.null ms

member :: (Eq a, Hashable a) => a -> MultiSet a -> Bool
member k (MultiSet ms) = M.member k ms

toList :: MultiSet a -> [a]
toList ms = concatMap go (toOccurList ms)
  where
    go (k, num) = take num $ repeat k

insert :: (Eq a, Hashable a) => a -> MultiSet a -> MultiSet a
insert k (MultiSet ms) | Just c <- M.lookup k ms
                       = MultiSet $ M.insert k (c + 1) ms
insert k (MultiSet ms) | otherwise
                       = MultiSet $ M.insert k 1 ms

singleton :: (Eq a, Hashable a) => a -> MultiSet a
singleton k = MultiSet (M.singleton k 1)

fromList  :: (Eq a, Hashable a) => [a] -> MultiSet a
fromList = foldl (flip insert) empty

toSet :: MultiSet a -> S.HashSet a
toSet (MultiSet ms) = M.keysSet ms
