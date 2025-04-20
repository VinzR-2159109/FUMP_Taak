module PrimitiveSimilarity
  ( Similarity(..)
  , Shape(..)
  , area
  ) where

import Data.Char (toLower, isLetter, ord)
import Data.List (nub, intersect)

-- Type class definition
class Similarity a where
    similarity :: a -> a -> Double
    distance :: a -> a -> Double

-- Int
instance Similarity Int where
    similarity a b = let maxVal = max a b in fromIntegral (min a b) / fromIntegral maxVal
    distance a b = fromIntegral $ abs (a - b)

-- String
instance Similarity String where
    similarity s1 s2 =
        let dist = distance s1 s2
            s1_l = map toLower s1
            s2_l = map toLower s2
            maxDist = fromIntegral $ max (length s1_l) (length s2_l) * 25
        in (maxDist - dist) / maxDist

    distance s1 s2 =
        let s1_l = map toLower s1
            s2_l = map toLower s2
        in fromIntegral $ sum (zipWith (\a b -> abs (ord a - ord b)) s1_l s2_l)

-- Shape
data Shape = Circle Double | Rectangle Double Double | Square Double deriving (Show, Eq)

instance Similarity Shape where
    similarity s1 s2 =
        let dist = distance s1 s2
            a1 = area s1
            a2 = area s2
            maxA = max a1 a2
        in (maxA - dist) / maxA

    distance s1 s2 = abs (area s1 - area s2)

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Square s) = s * s

-- 2D Point (Double, Double)
instance Similarity (Double, Double) where
    similarity p1 p2 =
        let dist = distance p1 p2
            maxDist = sqrt 2
        in (maxDist - dist) / maxDist

    distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- 2D Point (Integer, Integer)
instance Similarity (Integer, Integer) where
    similarity p1 p2 =
        let dist = distance p1 p2
            maxDist = sqrt 2
        in (maxDist - dist) / maxDist

    distance (x1, y1) (x2, y2) =
        sqrt (fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2)
