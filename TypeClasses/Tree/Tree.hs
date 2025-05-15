import Metric
import TreeStructure

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



