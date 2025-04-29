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
  in round gas

strategie_2 :: [Maanlander] -> Integer
strategie_2 [] = 0
strategie_2 (ml:_) =
  let v         = snelheid ml
      h         = hoogte ml
      g         = valversnelling ml
      maxRem    = motorkracht ml
      fuel      = brandstof ml
      safeSpeed = maxSnelheid ml

      urgency
        | h < 30 = 1.5
        | h < 100 = 1.2
        | otherwise = 1.0

      requiredBreak = (v * v) / (2 * h)
      break = urgency * requiredBreak

      gas
        | v > safeSpeed = min maxRem (min break fuel)
        | otherwise = 0
  in round gas

strategie_3 :: [Maanlander] -> Integer
strategie_3 [] = 0
strategie_3 (ml:_) =
  let v         = snelheid ml
      g         = valversnelling ml
      maxRem    = motorkracht ml
      fuel      = brandstof ml
      safeSpeed   = maxSnelheid ml

      safetyFactor
        | hoogte ml < 30 = 1.5
        | hoogte ml < 100 = 1.2
        | otherwise = 1.0

      targetV = safeSpeed / safetyFactor
      break = v + g - targetV
      
      gas = min maxRem (min break fuel)
  in round gas


updateLander :: Maanlander -> Integer -> Maanlander
updateLander ml gas =
  let a     = valversnelling ml - fromIntegral gas
      v' = fromIntegral (round ((snelheid ml + a) * 100)) / 100
      h'    = hoogte ml - v'
      fuel' = brandstof ml - fromIntegral gas
  in ml { hoogte = max 0 h', snelheid = v', brandstof = max 0 fuel' }

simulate :: Strategie -> Maanlander -> [Maanlander]
simulate strategie init = simulate' [init]
  where
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

          lander = Maanlander h 0 f m ms g
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
