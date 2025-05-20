{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module City where

import Metric
import Data.Char (toLower, ord)

data City = City
  { name        :: String
  , location    :: (Double, Double)
  , inhabitants :: Integer
  , shape       :: Shape
  }
  deriving (Show, Eq)

instance Metric String where
  similarity s1 s2 =
    let s1_l   = map toLower s1
        s2_l   = map toLower s2
        dist   = distance s1 s2
        maxDist = fromIntegral (max (length s1_l) (length s2_l) * 25)
    in (maxDist - dist) / maxDist

  distance s1 s2 =
    let s1_l = map toLower s1
        s2_l = map toLower s2
    in fromIntegral $ sum (zipWith (\a b -> abs (ord a - ord b)) s1_l s2_l)

instance Metric (Double, Double) where
  similarity p1 p2 =
    let dist    = distance p1 p2
        maxDist = sqrt 2
    in (maxDist - dist) / maxDist

  distance (x1,y1) (x2,y2) =
    sqrt ((x1 - x2)^2 + (y1 - y2)^2)

instance Metric (Integer, Integer) where
  similarity p1 p2 =
    let dist    = distance p1 p2
        maxDist = sqrt 2
    in (maxDist - dist) / maxDist

  distance (x1,y1) (x2,y2) =
    sqrt . fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2

data Shape
  = Circle Double
  | Rectangle Double Double
  | Square Double
  deriving (Show, Eq)

instance Metric Shape where
  similarity s1 s2 =
    let a1   = area s1
        a2   = area s2
        maxA = max a1 a2
    in (maxA - distance s1 s2) / maxA

  distance s1 s2 = abs (area s1 - area s2)

area :: Shape -> Double
area (Circle r)      = pi * r * r
area (Rectangle w h) = w * h
area (Square s)      = s * s

instance Metric City where
  distance c1 c2 =
    let dLoc  = distance (location c1) (location c2)
        dShp  = distance (shape c1)    (shape c2)
        dPop  = abs (fromIntegral (inhabitants c1) - fromIntegral (inhabitants c2))
    in dLoc + dShp + dPop

  similarity c1 c2 =
    let sLoc   = similarity (location c1) (location c2)
        sShp   = similarity (shape c1)    (shape c2)
        sPop   = let h1 = inhabitants c1
                     h2 = inhabitants c2
                 in fromIntegral (min h1 h2) / fromIntegral (max h1 h2)
    in (sLoc + sShp + sPop) / 3
