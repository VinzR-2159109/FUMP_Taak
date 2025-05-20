module Person.Personality (Personality(..)) where

import Metric   (Metric(..))

data Personality
  = Red     -- Dominant / Direct
  | Yellow  -- Influential / Enthusiastic
  | Green   -- Steady / Supportive
  | Blue    -- Conscientious / Analytical
  deriving (Eq, Show)

instance Metric Personality where
  similarity a b
    | a == b        = 1.0
    | bothMatch     = 0.6
    | eitherMatch   = 0.5
    | otherwise     = 0.0
    where
        bothMatch     = sameExtro a b && sameFocus a b
        eitherMatch   = sameExtro a b || sameFocus a b

        sameExtro x y = (x `elem` [Red,Yellow]) == (y `elem` [Red,Yellow])
        sameFocus x y = (x `elem` [Red,Blue])   == (y `elem` [Red,Blue])
