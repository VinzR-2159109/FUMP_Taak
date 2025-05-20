module Person.Ethnicity (Ethnicity(..)) where
import Metric

data Ethnicity
  = Asian
  | Black
  | Hispanic
  | White
  deriving (Eq, Show)

instance Metric Ethnicity where
  similarity x y
    | x == y    = 1.0
    | otherwise = 0.0
