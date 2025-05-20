{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tree( Tree(..), treeDepth, nodeCount, structureDistance, nodeContentDistance) where
import Metric

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show, Eq)
instance Metric Int where
    similarity x y = if x == y then 1 else 0
    distance x y   = 1 - similarity x y

treeDepth :: Tree a -> Int
treeDepth (Leaf _) = 1
treeDepth (Node _ l r) = 1 + max (treeDepth l) (treeDepth r)

nodeCount :: Tree a -> Int
nodeCount (Leaf _) = 1
nodeCount (Node _ l r) = 1 + nodeCount l + nodeCount r

structureDistance :: Tree a -> Tree a -> Double
structureDistance (Leaf _) (Leaf _) = 0
structureDistance (Leaf _) (Node {}) = 1
structureDistance (Node {}) (Leaf _) = 1
structureDistance (Node _ l1 r1) (Node _ l2 r2) = structureDistance l1 l2 + structureDistance r1 r2

nodeContentDistance :: (a -> a -> Double) -> Tree a -> Tree a -> Double
nodeContentDistance sim (Leaf a) (Leaf b) = 1 - sim a b
nodeContentDistance _ (Leaf _) (Node {}) = 1
nodeContentDistance _ (Node {}) (Leaf _) = 1
nodeContentDistance sim (Node a l1 r1) (Node b l2 r2) =
    (1 - sim a b) + nodeContentDistance sim l1 l2 + nodeContentDistance sim r1 r2

instance Metric a => Metric (Tree a) where
    distance :: Metric a => Tree a -> Tree a -> Double
    distance t1 t2 =
        let dDepth = abs (treeDepth t1 - treeDepth t2)
            dShape = structureDistance t1 t2
            dNodeContent = nodeContentDistance similarity t1 t2
        in fromIntegral dDepth + dShape + dNodeContent

    similarity :: Metric a => Tree a -> Tree a -> Double
    similarity t1 t2 =
        let dist = distance t1 t2
            maxPossibleDist = fromIntegral $ treeDepth t1 + treeDepth t2 + nodeCount t1 + nodeCount t2
        in (maxPossibleDist - dist) / maxPossibleDist
