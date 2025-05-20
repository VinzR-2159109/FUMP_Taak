module Main where

import Metric

import Tree
import Image

import Person.Person       (Person(..))
import Person.Age          (Age(..))
import Person.Gender       (Gender(..))
import Person.Ethnicity    (Ethnicity(..))
import Person.Education    (Education(..))
import Person.Personality  (Personality(..))

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
  -- Persons
  let personA = Person
        { name        = "Alice"
        , age         = Age 30
        , gender      = Female
        , ethnicity   = Asian
        , education   = Bachelors
        , personality = Red
        }
      personB = Person
        { name        = "Bob"
        , age         = Age 35
        , gender      = Male
        , ethnicity   = White
        , education   = Masters
        , personality = Yellow
        }
      personC = Person
        { name        = "Carol"
        , age         = Age 28
        , gender      = Female
        , ethnicity   = Hispanic
        , education   = Secondary
        , personality = Blue
        }
      people = [personA, personB, personC]

  putStrLn "\n=== Persons ==="
  putStrLn $ "distance Alice↔Bob:   " ++ show (distance personA personB)
  putStrLn $ "similarity Alice↔Bob: " ++ show (similarity personA personB)
  putStrLn $ "filterSimilar (>70% to Alice): " ++ show (filterSimilar people 70 personA)
  putStrLn $ "checkTriangle: " ++ show (checkTriangle people)
  putStrLn $ "canReach Alice→Carol (maxStep=0.2): " ++ show (canReach people personA personC 0.2)

  -- Trees
  let t1 = Node 1 (Leaf 2) (Leaf 3)                    :: Tree Int
      t2 = Node 1 (Node 2 (Leaf 4) (Leaf 5)) (Leaf 3)  :: Tree Int
      t3 = Leaf 2                                      :: Tree Int
      trees = [t1, t2, t3]

  putStrLn "\n=== Trees ==="
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

  putStrLn "\n=== Images ==="
  putStrLn $ "  distance img1 img2:   " ++ show (distance img1 img2)
  putStrLn $ "  similarity img1 img2: " ++ show (similarity img1 img2)
  putStrLn $ "  filterSimilar (>90% to img1): " ++ show (filterSimilar images 90 img1)
  putStrLn $ "  checkTriangle: " ++ show (checkTriangle images)
  putStrLn $ "  canReach img1→img2 with maxStep=0.05: " ++ show (canReach images img1 img2 0.05)
