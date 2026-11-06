module Main where

import Graphics.Gloss (Display (InWindow), black)
import Graphics.Gloss.Interface.IO.Game (playIO)
import Levels (highScorePath, loadHighScore)
import Model (GameState(highScore), initialGame)
import View (renderGameIO)
import Controller (handleInputIO, updateGameIO)

window :: Display
window = InWindow "Pac-Man" (800, 800) (100, 100)

fps :: Int
fps = 60

main :: IO ()
main = do
    initialState <- initialGame
    savedScore <- loadHighScore highScorePath
    playIO
        window
        black
        fps
        (initialState { highScore = savedScore })
        renderGameIO
        handleInputIO
        updateGameIO