module Main where

import Metric
import City
import Tree
import Image

filterSimilar :: Metric a => [a] -> Double -> a -> [a]
filterSimilar elems pct ref =  filter (\e -> similarity e ref >= pct/100) elems

checkTriangle :: Metric a => [a] -> Bool
checkTriangle xs = and [distance a c  <= distance a b + distance b c | a <- xs, b <- xs, c <- xs]

-- Zou ook met een Set kunnen, maar hebben we niet gezien in de les
canReach :: (Metric a, Eq a) => [a] -> a -> a -> Double -> Bool
canReach allNodes start end maxStep = bfs [] [start]
  where
    getNeighbors x =[ y | y <- allNodes, let stepCost = 1 - similarity x y, stepCost <= maxStep]

    bfs _      []      = False
    bfs visited (q:qs)
      | q == end         = True
      | q `elem` visited = bfs visited qs
      | otherwise        =
          let visited' = q : visited
              neighs   = getNeighbors q
              queue'   = qs ++ neighs
          in bfs visited' queue'


main :: IO ()
main = do
  -- Cities
  let cityA = City "Alpha" (0.1,0.2) 10000 (Circle 5)
      cityB = City "Beta"  (0.15,0.25) 12000 (Circle 5.2)
      cityC = City "Gamma" (0.8,0.9)   4000  (Square 3)
      cities = [cityA, cityB, cityC]

  putStrLn "Cities:"
  putStrLn $ "  filterSimilar (>80% to Alpha): " ++ show (filterSimilar cities 80 cityA)
  putStrLn $ "  checkTriangle: " ++ show (checkTriangle cities)
  putStrLn $ "  canReach Alpha→Gamma with maxStep=0.2: " ++ show (canReach cities cityA cityC 0.2)

  -- Trees
  let t1 = Node 1 (Leaf 2) (Leaf 3)                    :: Tree Int
      t2 = Node 1 (Node 2 (Leaf 4) (Leaf 5)) (Leaf 3)  :: Tree Int
      t3 = Leaf 2                                      :: Tree Int
      trees = [t1, t2, t3]

  putStrLn "\nTrees:"
  putStrLn $ "  Depths: " ++ show (treeDepth t1) ++ " vs " ++ show (treeDepth t2)
  putStrLn $ "  Node counts: " ++ show (nodeCount t1) ++ " vs " ++ show (nodeCount t2)
  putStrLn $ "  structureDistance t1 t2: " ++ show (structureDistance t1 t2)
  putStrLn $ "  nodeContentDistance t1 t2: " ++ show (nodeContentDistance similarity t1 t2)
  putStrLn $ "  distance t1 t2: " ++ show (distance t1 t2)
  putStrLn $ "  similarity t1 t2: " ++ show (similarity t1 t2)
  putStrLn $ "  filterSimilar (>50% to t1): " ++ show (filterSimilar trees 50 t1)
  putStrLn $ "  checkTriangle: " ++ show (checkTriangle trees)
  putStrLn $ "  canReach t1→t3 with maxStep=0.5: " ++ show (canReach trees t1 t3 0.5)

  -- Images
  let img1 = Image{ width  = 2, height = 2, pixels = [ [(0,0,0),(255,255,255)], [(255,0,0),(0,255,0)]]}
      img2 = Image{ width  = 2, height = 2, pixels = [ [(0,0,0),(250,250,250)], [(250,0,0),(0,250,0)]]}
      images = [img1, img2]

  putStrLn "\nImages example:"
  putStrLn $ "  distance img1 img2:   " ++ show (distance img1 img2)
  putStrLn $ "  similarity img1 img2: " ++ show (similarity img1 img2)
  putStrLn $ "  filterSimilar (>90% to img1): " ++ show (filterSimilar images 90 img1)
  putStrLn $ "  checkTriangle: " ++ show (checkTriangle images)
  putStrLn $ "  canReach img1→img2 with maxStep=0.05: " ++ show (canReach images img1 img2 0.05)
