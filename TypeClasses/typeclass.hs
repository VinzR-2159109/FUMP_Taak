{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use record patterns" #-}

import TreeStructure
import PrimitiveSimilarity

data City = City
  { name        :: String
  , location    :: (Double, Double)
  , inhabitants :: Integer
  , shape       :: Shape
  } deriving (Show, Eq)

instance Similarity City where
    distance c1 c2 =
        let d1 = distance (location c1) (location c2)
            d2 = distance (shape c1) (shape c2)
            i1 = inhabitants c1
            i2 = inhabitants c2
            d3 = abs (fromIntegral i1 - fromIntegral i2)
        in d1 + d2 + d3

    similarity c1 c2 =
        let sLoc  = similarity (location c1) (location c2)
            sShape = similarity (shape c1) (shape c2)
            h1 = inhabitants c1
            h2 = inhabitants c2
            sPop = fromIntegral (min h1 h2) / fromIntegral (max h1 h2)
        in (sLoc + sShape + sPop) / 3

instance Similarity a => Similarity (Tree a) where
    distance t1 t2 =
        let dDepth = abs (treeDepth t1 - treeDepth t2)
            dShape = structureDistance t1 t2
            dNodeContent = nodeContentDistance similarity t1 t2
        in fromIntegral dDepth + dShape + dNodeContent

    similarity t1 t2 =
        let dist = distance t1 t2
            maxPossibleDist = fromIntegral $ treeDepth t1 + treeDepth t2 + nodeCount t1 + nodeCount t2
        in (maxPossibleDist - dist) / maxPossibleDist


main :: IO ()
main = do
    let cityA = City "Alpha" (0.1, 0.2) 10000 (Circle 5)
    let cityB = City "Beta"  (0.15, 0.25) 12000 (Circle 5.2)
    let cityC = City "Gamma" (0.8, 0.9) 4000 (Square 3)

    let tree1 = Node cityA (Leaf cityB) (Leaf cityC)
    let tree2 = Node cityA (Leaf cityA) (Leaf cityB)

    putStrLn "City similarity (Alpha vs Beta):"
    print $ similarity cityA cityB

    putStrLn "\nTree similarity (tree1 vs tree2):"
    print $ similarity tree1 tree2

    putStrLn "\nTree distance (tree1 vs tree2):"
    print $ distance tree1 tree2
