module Person.Education (Education(..)) where
import Metric (Metric(..), distance)

data Education
  = Primary      -- 0
  | Secondary    -- 1
  | Bachelors    -- 2
  | Masters      -- 3
  | PhD          -- 4
  deriving (Eq, Show)

rank :: Education -> Int
rank lvl = case lvl of
  Primary   -> 0
  Secondary -> 1
  Bachelors -> 2
  Masters   -> 3
  PhD       -> 4

instance Metric Education where
  distance x y = fromIntegral (abs (rank x - rank y))

  similarity x y =
    let d    = distance x y
        maxD = fromIntegral (rank PhD - rank Primary)
    in (maxD - d) / maxD
