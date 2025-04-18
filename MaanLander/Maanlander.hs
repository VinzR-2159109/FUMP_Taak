import System.Environment (getArgs)

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

      -- a = v^2 / (2h)
      benodigdeRem = (v * v) / (2 * h)

      gewensteVersnelling = min benodigdeRem (v + g)

      -- Beperkingen: tegengas <= motorkracht && <= brandstof
      gas = min maxRem (min gewensteVersnelling fuel)
  in round gas

strategie_3 :: [Maanlander] -> Integer
strategie_3 [] = 0
strategie_3 (ml:rest) = round (max 0 (min maxGas adjustedGas))
  where
    history = ml : rest

    kp = 0.15
    ki = 0.01
    kd = 0.01

    maxGas = min (motorkracht ml) (brandstof ml)
    v_current = snelheid ml
    target_speed = maxSnelheid ml / 2

    e_current = v_current - target_speed
    e_integral = sum (map (\m -> snelheid m - target_speed) history)
    e_derivative = case rest of
                     (prev:_) -> v_current - snelheid prev
                     []       -> 0

    pid_output = kp * e_current + ki * e_integral + kd * e_derivative
    rawGas = pid_output + valversnelling ml

    adjustedGas
      | v_current - rawGas <= 0 = 0
      | otherwise = min rawGas (valversnelling ml + v_current - 0.1)


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
      let strategie = case (round nr :: Int) of
                        1 -> strategie_1
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
