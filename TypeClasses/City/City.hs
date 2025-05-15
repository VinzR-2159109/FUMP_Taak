module City (City(..), Shape(..)) where

import Metric (Metric(..))
import Data.Function   (on)
import Data.List       (maximumBy)
import Data.Ord        (comparing)
import Data.Foldable   (foldl')

data Shape
  = Circle Double     -- radius
  | Square Double     -- side length
  deriving (Eq, Show)

data City = City
  { cityName   :: String
  , location   :: (Double, Double)
  , population :: Int
  , footprint  :: Shape
  }
  deriving (Eq, Show)

euclid :: (Double,Double) -> (Double,Double) -> Double
euclid (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

normalize :: Double -> Double -> Double
normalize vmax x = min 1 (x / vmax)

instance Metric City where
  similarity a b =
    let
        geoDist = euclid (location a) (location b)
        sLoc    = 1 / (1 + geoDist)

        maxPop = 20000 
        popDist = abs (fromIntegral (population a - population b))
        sPop    = 1 - normalize maxPop popDist

        area (Circle r) = pi * r*r
        area (Square s) = s*s
        a1 = area (footprint a)
        a2 = area (footprint b)
        sShape = min a1 a2 / max a1 a2

        wLoc   = 0.5
        wPop   = 0.3
        wShape = 0.2
    in  wLoc*sLoc + wPop*sPop + wShape*sShape

  distance a b = 1 - similarity a b
