module Person.Age (Age(..)) where
import Metric

newtype Age = Age Int
  deriving (Eq, Show)

instance Metric Age where
  distance (Age a) (Age b) = fromIntegral (abs (a - b))
  similarity (Age a) (Age b) = 1 / (1 + distance (Age a) (Age b))
