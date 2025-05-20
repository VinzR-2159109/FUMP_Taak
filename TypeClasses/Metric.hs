module Metric(Metric(..))where

class Metric a where
    similarity :: a -> a -> Double
    distance :: a -> a -> Double