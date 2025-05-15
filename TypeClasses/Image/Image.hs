data Image = Image
  { width  :: Int
  , height :: Int
  , pixels :: [[(Int,Int,Int)]]
  } deriving (Eq, Show)

class Similarity a where
    similarity :: a -> a -> Double
    distance :: a -> a -> Double
    
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