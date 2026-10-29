module Main where

import Graphics.Gloss (Display (InWindow), black)
import Graphics.Gloss.Interface.IO.Game (playIO)
import Model (initialGame)
import View (renderGameIO)
import Controller (handleInputIO, updateGameIO)

window :: Display
window = InWindow "Pac-Man" (800, 800) (100, 100)

fps :: Int
fps = 60

main :: IO ()
main = do
    initialState <- initialGame
    playIO
        window
        black
        fps
        initialState
        renderGameIO
        handleInputIO
        updateGameIO