module PathFinder.PathFinder (path, isConnected) where



import Control.Applicative
import Data.Maybe
import Data.List           hiding (insert)
import Data.Map            hiding ((\\), foldr)
import PathFinder.Graph



type VisitedNodes a = Map a (Int, Maybe a)



path :: Ord a => Graph a -> a -> a -> Maybe [a]
path g from to = buildPath to <$> extendMapUntil foundFrom g (emptyMap to)
  where foundFrom = (from `member`)

isConnected :: Ord a => Graph a -> Bool
isConnected g = isJust $ extendMapUntil isFull g (emptyMap start)
  where isFull m = length (keys m) == length (nodes g)
        start = label (head $ nodes g)



emptyMap :: Ord a => a -> VisitedNodes a
emptyMap s = singleton s (0, Nothing)

extendMap :: Ord a => Graph a -> a -> VisitedNodes a -> VisitedNodes a
extendMap g n m =
  let (l, _) = m ! n
      ps = parents g n
      ns = ps \\ keys m in
  foldr (\p -> insert p (l + 1, Just n)) m ns

extendMapUntil :: Ord a => (VisitedNodes a -> Bool) -> Graph a -> VisitedNodes a -> Maybe (VisitedNodes a)
extendMapUntil p g m
  | p m       = Just m
  | otherwise = let m' = foldr (extendMap g) m (keys m) in
                    if m' == m then Nothing else extendMapUntil p g m'

buildPath :: Ord a => a -> VisitedNodes a -> [a]
buildPath n m = let (_, mp) = m ! n in
                 case mp of
                  Nothing -> [n]
                  Just p  -> n : buildPath p m

