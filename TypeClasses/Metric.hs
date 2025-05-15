module Metric
  ( Metric(..)
  , filterSimilar
  , checkTriangle
  , canReach
  ) where


class Metric a where
  similarity :: a -> a -> Double
  distance   :: a -> a -> Double
  distance x y = 1 - similarity x y

filterSimilar :: Metric a => [a] -> Double -> a -> [a]
filterSimilar elems pct ref =
  filter (\e -> similarity e ref >= pct/100) elems

checkTriangle :: Metric a => [a] -> Bool
checkTriangle xs =
  all (\(a,b,c) -> distance a c <= distance a b + distance b c)
      [ (a,b,c) | a <- xs, b <- xs, c <- xs ]

canReach :: (Metric a, Eq a) => [a] -> a -> a -> Double -> Bool
canReach allNodes start end maxStep = bfs [] [start]
  where
    getNeighbors x =
      [ y | y <- allNodes
          , let stepCost = distance x y
          , stepCost <= maxStep
      ]

    bfs _      []     = False
    bfs visited (q:qs)
      | q == end            = True
      | q `elem` visited    = bfs visited qs
      | otherwise           =
          let visited' = q : visited
              queue'   = qs ++ getNeighbors q
          in bfs visited' queue'
