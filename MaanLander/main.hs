{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Maanlander
import Result

import qualified Graphics.UI.Threepenny as UI
import qualified Text.Read as TR

import Graphics.UI.Threepenny.Core
import Control.Monad (void)
import Text.Read (readMaybe)
import System.Process (callCommand)
import System.Directory (doesFileExist)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Prelude hiding (init)

main :: IO ()
main = startGUI defaultConfig { jsStatic = Just "static" } setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Maanlander Simulator"

    -- Strategy dropdown
    selectStrategy <- UI.select #+ map (\s -> UI.option #+ [string s]) ["Strategie 1", "Strategie 2", "Strategie 3"]

    -- Sliders
    (sliderHeight, valueHeight) <- mkSliderWithValue (0, 800) 500
    (sliderFuel, valueFuel) <- mkSliderWithValue (0, 1000) 200
    (sliderMotor, valueMotor) <- mkSliderWithValue (0, 20) 5
    (sliderSafeSpeed, valueSafeSpeed) <- mkSliderWithValue (0, 50) 10

    -- Gravity dropdown (Planets)
    selectGravity <- UI.select #+ map (\s -> UI.option #+ [string s]) 
      ["Mercury (3.7)", "Venus (8.87)", "Earth (9.81)",
      "Moon (1.62)", "Mars (3.71)" , "Jupiter (24.79)",
      "Saturn (10.44)", "Uranus (8.69)", "Neptune (11.15)"]

    -- Simulate button
    btnSimulate <- UI.button #+ [string "Simulate"]

    -- Message area
    message <- UI.span

    -- Layout
    getBody window #+
      [ UI.div # set UI.style[ ("display", "flex"), ("justify-content", "center"), ("align-items", "center"), ("height", "100vh"), ("flex-direction", "column")] #+
          [ column
            [   UI.h1 # set UI.text "Maanlander Simulator"
              , UI.div #+ [UI.string "Strategie:", element selectStrategy] # set UI.style [("display", "flex"), ("gap", "10px"), ("align-items", "center")]
              , UI.div #+ [UI.string "Start hoogte (m):", element sliderHeight, element valueHeight] # set UI.style [("display", "flex"), ("gap", "10px"), ("align-items", "center")]
              , UI.div #+ [UI.string "Brandstof (l):", element sliderFuel, element valueFuel] # set UI.style [("display", "flex"), ("gap", "10px"), ("align-items", "center")]
              , UI.div #+ [UI.string "Max motorkracht:", element sliderMotor, element valueMotor] # set UI.style [("display", "flex"), ("gap", "10px"), ("align-items", "center")]
              , UI.div #+ [UI.string "Max veilige snelheid (m/s):", element sliderSafeSpeed, element valueSafeSpeed] # set UI.style [("display", "flex"), ("gap", "10px"), ("align-items", "center")]
              , UI.div #+ [UI.string "Valversnelling:", element selectGravity] # set UI.style [("display", "flex"), ("gap", "10px"), ("align-items", "center")]
              , element btnSimulate # set UI.style [("padding", "10px"), ("font-size", "20px"), ("background-color", "#4CAF50"), ("color", "white"), ("border", "none"), ("border-radius", "5px")]
              , element message
            ] # set UI.style [("display", "flex"),("flex-direction", "column"), ("gap", "10px"), ("align-items", "left")] 
          ]
      ]

    on UI.click btnSimulate $ \_ -> do
      stratIndex <- get UI.selection selectStrategy

      let mStrategy = fmap (+1) stratIndex

      mHeight <- fmap Text.Read.readMaybe (get value sliderHeight) 
      mFuel   <- fmap Text.Read.readMaybe (get value sliderFuel)
      mMotor  <- fmap Text.Read.readMaybe (get value sliderMotor) 
      mSafe   <- fmap Text.Read.readMaybe (get value sliderSafeSpeed) 
      mGrav <- do
        selected <- get UI.selection selectGravity
        let gravities = [3.7, 8.87, 9.81, 1.62, 3.71, 24.79, 10.44, 8.69, 11.15]
        return $ fmap (gravities !!) selected

      case (mStrategy, mHeight, mFuel, mMotor, mSafe, mGrav) of
        (Just strat, Just h, Just f, Just m, Just s, Just g) -> do
          let strategie = case strat of
                            1 -> strategie_1
                            2 -> strategie_2
                            3 -> strategie_3
                            _ -> strategie_1

              initLander = Maanlander h 0 f m s g 0
              result = simulate strategie initLander
              final  = last result
              landingMsg =
                if abs (snelheid final) <= maxSnelheid final
                  then "✅ Success: Landed safely!"
                  else "❌ Failed: Crashed too hard."

              outputLines = map show result ++ ["", landingMsg]
              outputText = unlines outputLines

          liftIO $ writeFile "maanlander_output.txt" outputText
          let finalMessage = landingMsg
          showRocketLanding window result finalMessage

        _ -> void $ element message # set UI.text "❌ Please fill in all input fields correctly."

-- Helpers
mkSliderWithValue :: (Int, Int) -> Int -> UI (Element, Element)
mkSliderWithValue (minV, maxV) start = do
  slider <- UI.input
    # set UI.type_ "range"
    # set (attr "min") (show minV)
    # set (attr "max") (show maxV)
    # set (attr "value") (show start)

  valueDisplay <- UI.span # set UI.text (show start)
  on UI.valueChange slider (\val -> element valueDisplay # set UI.text val)

  return (slider, valueDisplay)
