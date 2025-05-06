import TreeStructure
import PrimitiveSimilarity

data City = City
  { name        :: String
  , location    :: (Double, Double)
  , inhabitants :: Integer
  , shape       :: Shape
  } deriving (Show, Eq)

data Image = Image
  { width  :: Int
  , height :: Int
  , pixels :: [[(Int,Int,Int)]]
  } deriving (Eq, Show)


instance Similarity City where
    distance :: City -> City -> Double
    distance c1 c2 =
        let d1 = distance (location c1) (location c2)
            d2 = distance (shape c1) (shape c2)
            i1 = inhabitants c1
            i2 = inhabitants c2
            d3 = abs (fromIntegral i1 - fromIntegral i2)
        in d1 + d2 + d3

    similarity :: City -> City -> Double
    similarity c1 c2 =
        let sLoc  = similarity (location c1) (location c2)
            sShape = similarity (shape c1) (shape c2)
            h1 = inhabitants c1
            h2 = inhabitants c2
            sPop = fromIntegral (min h1 h2) / fromIntegral (max h1 h2)
        in (sLoc + sShape + sPop) / 3

instance Similarity a => Similarity (Tree a) where
    distance :: Similarity a => Tree a -> Tree a -> Double
    distance t1 t2 =
        let dDepth = abs (treeDepth t1 - treeDepth t2)
            dShape = structureDistance t1 t2
            dNodeContent = nodeContentDistance similarity t1 t2
        in fromIntegral dDepth + dShape + dNodeContent

    similarity :: Similarity a => Tree a -> Tree a -> Double
    similarity t1 t2 =
        let dist = distance t1 t2
            maxPossibleDist = fromIntegral $ treeDepth t1 + treeDepth t2 + nodeCount t1 + nodeCount t2
        in (maxPossibleDist - dist) / maxPossibleDist

instance Similarity Image where
    distance :: Image -> Image -> Double
    distance a b =
        let ps1 = concat (pixels a)
            ps2 = concat (pixels b)

            pairs = zip ps1 ps2

            pixelDist ((r1,g1,b1),(r2,g2,b2)) = sqrt . fromIntegral $ (r1-r2)^2 + (g1-g2)^2 + (b1-b2)^2
            
            total = sum (map pixelDist pairs)
            count = fromIntegral (length pairs)
        in total / count

    similarity :: Image -> Image -> Double
    similarity a b =
        let d = distance a b
            dmax = sqrt (3 * 255^2)
        in 1 - d / dmax

filterSimilar :: Similarity a => [a] -> Double -> a -> [a]
filterSimilar elems pct ref =  filter (\e -> similarity e ref >= pct/100) elems


checkTriangle :: Similarity a => [a] -> Bool
checkTriangle xs = and [distance a c  <= distance a b + distance b c | a <- xs, b <- xs, c <- xs]

-- Zou ook met een Set kunnen, maar hebben we niet gezien in de les
canReach :: (Similarity a, Eq a) => [a] -> a -> a -> Double -> Bool
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
    -- voorbeeld met steden
    let cityA = City "Alpha" (0.1,0.2) 10000 (Circle 5)
        cityB = City "Beta"  (0.15,0.25) 12000 (Circle 5.2)
        cityC = City "Gamma" (0.8,0.9)   4000  (Square 3)
        cities = [cityA, cityB, cityC]

    putStrLn $ "filterSimilar: " ++ "\n" ++ show (filterSimilar cities 80 cityA)

    putStrLn $ "checkTriangle: " ++ show (checkTriangle cities)

    putStrLn $ "canReach: " ++ show (canReach cities cityA cityC 0.2)