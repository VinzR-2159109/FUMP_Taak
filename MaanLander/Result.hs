{-# LANGUAGE OverloadedStrings #-}

module Result where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Control.Monad (void)

showResult :: Window -> String -> UI ()
showResult window outputText = do
    getBody window # set children [] -- Clear page

    backButton <- UI.button #+ [string "Back"]
    element backButton # set UI.style
      [ ("margin-top", "20px")
      , ("background-color", "#4CAF50")
      , ("color", "white")
      , ("padding", "10px")
      , ("border", "none")
      , ("border-radius", "5px")
      , ("font-size", "20px")
      , ("cursor", "pointer")
      ]

    on UI.click backButton $ \_ -> runFunction $ ffi "location.reload()"

    void $ getBody window #+
        [ UI.h1 # set UI.text "Maanlander Simulation Result"
        , UI.pre # set UI.text outputText
        , element backButton
        ]

