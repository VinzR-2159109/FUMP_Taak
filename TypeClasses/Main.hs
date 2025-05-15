module Main where

import Metric
import City

main :: IO ()
main = do
  let cityA = City "Alpha" (0.1,0.2) 10000 (Circle 5)
      cityB = City "Beta"  (0.15,0.25) 12000 (Circle 5.2)
      cityC = City "Gamma" (0.8,0.9)   4000  (Square 3)
      cities = [cityA, cityB, cityC]

  putStrLn "filterSimilar (≥80% similar to Alpha):"
  print $ filterSimilar cities 80 cityA

  putStrLn "\nTriangle inequality holds?"
  print $ checkTriangle cities

  putStrLn "\nCan reach Gamma from Alpha with maxStep=0.2?"
  print $ canReach cities cityA cityC 0.2
