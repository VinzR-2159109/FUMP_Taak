{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module TreeStructure
  ( Tree(..)
  , treeDepth
  , nodeCount
  , structureDistance
  , nodeContentDistance
  ) where

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show, Eq)

treeDepth :: Tree a -> Int
treeDepth (Leaf _) = 1
treeDepth (Node _ l r) = 1 + max (treeDepth l) (treeDepth r)

nodeCount :: Tree a -> Int
nodeCount (Leaf _) = 1
nodeCount (Node _ l r) = 1 + nodeCount l + nodeCount r

structureDistance :: Tree a -> Tree a -> Double
structureDistance (Leaf _) (Leaf _) = 0
structureDistance (Leaf _) (Node _ _ _) = 1
structureDistance (Node _ _ _) (Leaf _) = 1
structureDistance (Node _ l1 r1) (Node _ l2 r2) = structureDistance l1 l2 + structureDistance r1 r2

nodeContentDistance :: (a -> a -> Double) -> Tree a -> Tree a -> Double
nodeContentDistance sim (Leaf a) (Leaf b) = 1 - sim a b
nodeContentDistance _ (Leaf _) (Node _ _ _) = 1
nodeContentDistance _ (Node _ _ _) (Leaf _) = 1
nodeContentDistance sim (Node a l1 r1) (Node b l2 r2) =
    (1 - sim a b) + nodeContentDistance sim l1 l2 + nodeContentDistance sim r1 r2
