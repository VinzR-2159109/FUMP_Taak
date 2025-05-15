{-# LANGUAGE OverloadedStrings #-}

module Result where

import Maanlander
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Control.Monad
import qualified Data.IORef as IORef

-- Configuration
maxHeight :: Double
maxHeight = 800.0

padding :: Int
padding = 50

canvasHeight :: Int
canvasHeight = round maxHeight + 2 * padding

usableHeight :: Double
usableHeight = fromIntegral (canvasHeight - 2 * padding)

showRocketLanding :: Window -> [Maanlander] -> String -> UI ()
showRocketLanding window landerStates finalMessage = do
    getBody window # set children []

    canvas <- UI.canvas
        # set UI.width  400
        # set UI.height canvasHeight
        # set UI.style [("background-color","black")]

    label       <- UI.span
    resultLabel <- UI.span # set UI.style [("white-space","pre-wrap")]

    void $ getBody window #+ [element canvas, element label, UI.br, element resultLabel]

    -- remember the very first fuel level
    let initialFuel = brandstof (head landerStates)

    timer    <- UI.timer # set UI.interval 100
    countRef <- liftIO $ IORef.newIORef 0

    on UI.tick timer $ \_ -> do
        count <- liftIO $ IORef.readIORef countRef

        if count < length landerStates
        then do
            let ml     = landerStates !! count
                h      = hoogte ml
                yPos   = floor ( fromIntegral padding
                               + (1 - h / maxHeight) * usableHeight
                               ) - 40
                currFuel = brandstof ml

            UI.clearCanvas canvas

            drawFuelBar canvas initialFuel currFuel
            drawRocket  canvas ml yPos

            element resultLabel # set UI.text
              (  "Hoogte: " ++ show (hoogte ml) ++ " m\n"
              ++ "Snelheid: " ++ show (snelheid ml) ++ " m/s\n"
              ++ "Brandstof: " ++ show (brandstof ml) ++ " L"
              )

            liftIO $ IORef.modifyIORef' countRef (+1)
        else do
            let finalLander = last landerStates
                safeLanding = abs (snelheid finalLander) <= maxSnelheid finalLander
                yEnd        = canvasHeight - padding - 40

            UI.clearCanvas canvas

            if safeLanding
              then drawRocket    canvas finalLander yEnd
              else drawExplosion canvas yEnd

            element resultLabel # set UI.text finalMessage
            UI.stop timer

    UI.start timer


-- Draw rocket with optional flame
drawRocket :: UI.Canvas -> Maanlander -> Int -> UI ()
drawRocket canvas lander y = do
    drawScale canvas

    -- Rocket body
    UI.beginPath canvas
    UI.set' UI.fillStyle (UI.htmlColor "white") canvas
    UI.fillRect (190, fromIntegral y) 20 40 canvas

    -- Rocket nose
    UI.beginPath canvas
    UI.moveTo (190, fromIntegral y) canvas
    UI.lineTo (200, fromIntegral y - 20) canvas
    UI.lineTo (210, fromIntegral y) canvas
    UI.closePath canvas
    UI.fill canvas

    -- Flame if falling
    when (snelheid lander > 0 && hoogte lander > 0 && gas lander > 0) $ do
        UI.beginPath canvas
        UI.set' UI.fillStyle (UI.htmlColor "orange") canvas
        UI.moveTo (190, fromIntegral y + 40) canvas
        UI.lineTo (200, fromIntegral y + 60) canvas
        UI.lineTo (210, fromIntegral y + 40) canvas
        UI.closePath canvas
        UI.fill canvas

-- Draw explosion
drawExplosion :: UI.Canvas -> Int -> UI ()
drawExplosion canvas y = do
    drawScale canvas

    -- Explosion circle
    UI.beginPath canvas
    UI.set' UI.fillStyle (UI.htmlColor "red") canvas
    UI.arc (200, fromIntegral y) 30 0 (2 * pi) canvas
    UI.fill canvas

    -- Yellow core
    UI.beginPath canvas
    UI.set' UI.fillStyle (UI.htmlColor "yellow") canvas
    UI.arc (200, fromIntegral y) 15 0 (2 * pi) canvas
    UI.fill canvas

-- Draw height scale and ground line
drawScale :: UI.Canvas -> UI ()
drawScale canvas = do
    UI.beginPath canvas
    UI.set' UI.strokeStyle "white" canvas
    UI.set' UI.fillStyle (UI.htmlColor "white") canvas
    UI.set' UI.textFont "12px sans-serif" canvas

    -- Ground line
    UI.beginPath canvas
    UI.moveTo (100, fromIntegral (canvasHeight - padding)) canvas
    UI.lineTo (400, fromIntegral (canvasHeight - padding)) canvas
    UI.stroke canvas

    -- Height ticks and labels
    mapM_ (\h -> do
        let y = fromIntegral padding + (1 - h / maxHeight) * usableHeight
        UI.moveTo (30, y) canvas
        UI.lineTo (40, y) canvas
        UI.stroke canvas
        UI.fillText (show (round h) ++ "m") (5, y - 5) canvas
      ) [0,100..maxHeight]

drawFuelBar :: UI.Canvas -> Double -> Double -> UI ()
drawFuelBar canvas initialFuel currFuel = do
    let
        barW       = 200     :: Double
        barH       = 10      :: Double
        barX       = 10      :: Double
        barY       = fromIntegral canvasHeight - 2 * barH
        frac       = currFuel / initialFuel
        filledW    = frac * barW

    UI.beginPath canvas
    UI.set' UI.fillStyle (UI.htmlColor "grey") canvas
    UI.fillRect (barX, barY) barW barH canvas

    UI.beginPath canvas
    UI.set' UI.fillStyle (UI.htmlColor "lime") canvas
    UI.fillRect (barX, barY) filledW barH canvas
