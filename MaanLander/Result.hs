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
        # set UI.width 400
        # set UI.height canvasHeight
        # set UI.style [("background-color", "black")]

    label <- UI.span
    resultLabel <- UI.span # set UI.style [("white-space", "pre-wrap")]

    void $ getBody window #+ [element canvas, element label, UI.br, element resultLabel]

    timer <- UI.timer # set UI.interval 100
    countRef <- liftIO $ IORef.newIORef 0

    on UI.tick timer $ \_ -> do
        count <- liftIO $ IORef.readIORef countRef
        if count < length landerStates
        then do
            let lander = landerStates !! count
                h = hoogte lander
                yPos = floor (fromIntegral padding + (1 - h / maxHeight) * usableHeight) - 40

            UI.clearCanvas canvas
            drawRocket canvas lander yPos

            element label # set UI.text ("Hoogte: " ++ show h)
            element resultLabel # set UI.text (show lander)

            liftIO $ IORef.modifyIORef' countRef (+1)
        else do
            let finalLander = last landerStates
            let safeLanding = abs (snelheid finalLander) <= maxSnelheid finalLander

            UI.clearCanvas canvas

            if safeLanding
                then drawRocket canvas finalLander (canvasHeight - padding - 40)
                else drawExplosion canvas (canvasHeight - padding - 40)
            element label # set UI.text finalMessage

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
