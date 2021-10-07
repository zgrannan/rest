{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.REST.PartialOrder (
      empty
    , insert
    , replaceUnsafe
    , insertUnsafe
    , gt
    , toList
    , isEmpty
    , elems
    , unionDisjointUnsafe
    , PartialOrder
    , toDescsList
    , descendents
    ) where

import GHC.Generics (Generic)
import Debug.Trace
import Data.Hashable
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

import Language.REST.Types
import Text.Printf

newtype PartialOrder a = PartialOrder (M.Map a (S.Set a))
  deriving (Ord, Eq, Generic, Hashable)

instance (Show a) => Show (PartialOrder a) where
  show (PartialOrder m) = L.intercalate " ∧ " $ map go (M.toList m) where
    go (key, s) = case S.toList s of
      [x] -> printf "%s > %s" (show key) (show x)
      xs  -> printf "%s > { %s }" (show key) (L.intercalate ", " (map show xs))


empty = PartialOrder M.empty

isEmpty p = p == empty

canInsert :: (Eq a, Ord a, Hashable a) => PartialOrder a -> a -> a -> Bool
canInsert o f g = f /= g && not (gt o f g) && not (gt o g f)

gt :: (Eq a, Ord a, Hashable a) => PartialOrder a -> a -> a -> Bool
gt po t u = S.member u $ descendents t po

unionDisjointUnsafe (PartialOrder m) (PartialOrder m') = PartialOrder (M.union m m')

ascendants k (PartialOrder m)  = M.keysSet $ M.filter (S.member k) m
descendents k (PartialOrder m) = M.findWithDefault S.empty k m

{-# INLINE insertUnsafe #-}
insertUnsafe o@(PartialOrder m) f g = result
  where
    result = PartialOrder $ M.insertWith S.union f decs $ M.mapWithKey go m

    go k old | S.member k ascs = S.union old decs
    go _ v   | otherwise       = v

    ascs = ascendants f o
    decs = S.insert g $ descendents g o

{-# INLINE insert #-}
insert :: (Eq a, Ord a, Hashable a) => PartialOrder a -> a -> a -> Maybe (PartialOrder a)
insert o f g = if canInsert o f g then Just (insertUnsafe o f g) else Nothing

toDescsList (PartialOrder m) = M.toList m

toList :: PartialOrder a -> [(a, a)]
toList (PartialOrder m) = do
  (k, vs) <- M.toList m
  v       <- S.toList vs
  return (k, v)

elems :: (Eq a, Ord a, Hashable a) => PartialOrder a -> S.Set a
elems (PartialOrder m) = S.union (M.keysSet m) (S.unions (M.elems m))

replaceUnsafe :: (Eq a, Ord a, Hashable a) => [a] -> a -> PartialOrder a -> PartialOrder a
replaceUnsafe froms to po@(PartialOrder m) = result where

  from' = S.fromList froms

  descs = S.unions (map (`descendents` po) froms)

  filtered = M.filterWithKey (\k _ -> not $ k `elem` froms) m
  m' =
    if S.null descs
    then filtered
    else M.insertWith S.union to descs filtered

  result = PartialOrder $ M.map go m'

  go s | hasFrom s = S.insert to $ S.union descs $ S.difference s from'
  go s | otherwise = s

  hasFrom set = any (`S.member` set) froms
