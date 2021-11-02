module Language.REST.Util where

import qualified Data.List as L

removeEqBy :: (Eq a) => (a -> a -> Bool) -> [a] -> [a] -> ([a], [a])
removeEqBy _ [] ys = ([], ys)
removeEqBy f (x : xs) ys
  | Just y <- L.find (f x) ys
  = removeEqBy f xs $ L.delete y ys
  | otherwise
  = let (xs', ys') = removeEqBy f xs ys in (x : xs', ys')
