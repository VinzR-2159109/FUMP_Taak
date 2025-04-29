{-# LANGUAGE OverloadedStrings #-}

module Result where

import Maanlander
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Control.Monad (void)
import qualified Data.IORef as IORef

-- Configuration
maxHeight :: Double
maxHeight = 800.0

canvasHeight :: Int
canvasHeight = round (maxHeight + 100)

-- Main animation
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
                yPos = floor (fromIntegral canvasHeight - (h / maxHeight) * fromIntegral canvasHeight) - 40

            UI.clearCanvas canvas
            drawRocket canvas yPos

            element label # set UI.text ("Hoogte: " ++ show h)
            element resultLabel # set UI.text (show lander)

            liftIO $ IORef.modifyIORef' countRef (+1)
        else do
            element label # set UI.text finalMessage
            UI.stop timer

    UI.start timer

-- Draw rocket at a vertical position
drawRocket :: UI.Canvas -> Int -> UI ()
drawRocket canvas y = do
    UI.clearCanvas canvas
    drawScale canvas

    UI.beginPath canvas
    UI.set' UI.fillStyle (UI.htmlColor "white") canvas

    -- Rocket body
    UI.fillRect (190, fromIntegral y) 20 40 canvas

    -- Rocket nose
    UI.beginPath canvas
    UI.moveTo (190, fromIntegral y) canvas
    UI.lineTo (200, fromIntegral y - 20) canvas
    UI.lineTo (210, fromIntegral y) canvas
    UI.closePath canvas
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
    UI.moveTo (100, fromIntegral canvasHeight) canvas
    UI.lineTo (400, fromIntegral canvasHeight) canvas
    UI.stroke canvas

    -- Height ticks and labels
    mapM_ (\h -> do
        let y = fromIntegral canvasHeight - (h / maxHeight) * fromIntegral canvasHeight
        UI.moveTo (30, y) canvas
        UI.lineTo (40, y) canvas
        UI.stroke canvas
        UI.fillText (show (round h) ++ "m") (5, y - 5) canvas
      ) [0,100..maxHeight]
