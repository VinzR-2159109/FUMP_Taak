module Maanlander where

import System.Environment (getArgs)
import Debug.Trace (trace)
import Language.Haskell.TH (safe)

data Maanlander = Maanlander
  { hoogte     :: Double  -- meter
  , snelheid   :: Double  -- meter per seconde
  , brandstof  :: Double  -- liter
  , motorkracht :: Double -- maximale gas per seconde
  , maxSnelheid :: Double -- maximale veilige landingssnelheid
  , valversnelling :: Double -- m/s²
  , gas :: Integer -- remkracht in m/s
  } deriving (Show)

type Strategie = [Maanlander] -> Integer

strategie_1 :: [Maanlander] -> Integer
strategie_1 [] = 0
strategie_1 (ml:_) =
  let v         = snelheid ml
      h         = hoogte ml
      g         = valversnelling ml
      maxRem    = motorkracht ml
      fuel      = brandstof ml
      safeSpeed = maxSnelheid ml

      requiredBreak = (v * v) / (2 * h)
      break = min requiredBreak (v + g)

      gas = min maxRem (min break fuel)
  in ceiling gas

strategie_2 :: [Maanlander] -> Integer
strategie_2 [] = 0
strategie_2 (ml:_) =
  let v         = snelheid ml
      g         = valversnelling ml
      maxRem    = motorkracht ml
      fuel      = brandstof ml
      safeSpeed = maxSnelheid ml

      
      requiredBreak = (max 0 (v + g - safeSpeed))
      gas = min maxRem (min requiredBreak fuel)
  in ceiling gas


strategie_3 :: [Maanlander] -> Integer
strategie_3 [] = 0
strategie_3 (ml:_) =
  let v         = snelheid ml
      g         = valversnelling ml
      maxRem    = motorkracht ml
      fuel      = brandstof ml
      safeSpeed   = maxSnelheid ml

      safetyFactor
        | hoogte ml < 30 = 5
        | hoogte ml < 100 = 2
        | hoogte ml < 300 = 1
        | otherwise = 0.75

      targetV = safeSpeed / safetyFactor
      requiredBreak = v + g - targetV
      
      gas = min maxRem (min requiredBreak fuel)
  in round gas


updateLander :: Maanlander -> Integer -> Maanlander
updateLander ml gas =
  let a     = valversnelling ml - fromIntegral gas
      v' = fromIntegral (round ((snelheid ml + a) * 100)) / 100
      h'    = hoogte ml - v'
      fuel' = brandstof ml - fromIntegral gas
  in ml { hoogte = max 0 h', snelheid = v', brandstof = max 0 fuel', gas = gas }

simulate :: Strategie -> Maanlander -> [Maanlander]
simulate strategie init = simulate' [init]
  where
    simulate' [] = []
    simulate' (current:rest)
      | hoogte current <= 0 = reverse (current : rest)
      | otherwise =
          let gas = strategie (current : rest)
              next = updateLander current gas
          in simulate' (next : (current : rest))


main :: IO ()
main = do
  args <- getArgs
  case mapM readMaybe args of
    Just [nr, h, f, m, ms, g] -> do
      let strategie = case (nr :: Double) of
                        1 -> strategie_1
                        2 -> strategie_2
                        3 -> strategie_3
                        _ -> strategie_1

          lander = Maanlander h 0 f m ms g 0
          result = simulate strategie lander
          final  = last result
          landingMsg =
            if abs (snelheid final) <= maxSnelheid final
              then "Succes"
              else "Failed"

          outputLines = map show result ++ ["", landingMsg]

      writeFile "maanlander_output.txt" (unlines outputLines)
      putStrLn (landingMsg ++ " :: v_final: " ++ show (snelheid final))
    _ -> putStrLn "Usage: <strategie #> <hoogte in m> <brandstof in liter> <maxMotorkracht> <maxSnelheid> <valversnelling>"


readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(val, "")] -> Just val
  _           -> Nothing
