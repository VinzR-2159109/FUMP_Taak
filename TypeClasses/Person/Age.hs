module Person.Age () where
import Metric   (Metric(..))

instance Metric Int where
  distance a b = abs $ fromIntegral(a - b)
  similarity a b = 1 / (1 + distance a b)

