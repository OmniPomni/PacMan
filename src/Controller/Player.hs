module Controller.Player where

import Graphics.Gloss.Interface.IO.Game (Event)
import Model (PacMan(..), Direction(..), Position, Grid)
import qualified Model

bufferInput :: Event -> PacMan -> PacMan
bufferInput (EventKey (SpecialKey KeyUp) Down _ _) pacman = pacman {queuedDir = Just Model.Up}
bufferInput (EventKey (SpecialKey KeyDown) Down _ _) pacman = pacman {queuedDir = Just Model.Down}
bufferInput (EventKey (SpecialKey KeyLeft) Down _ _) pacman = pacman {queuedDir = Just Model.Left}
bufferInput (EventKey (SpecialKey KeyRight) Down _ _) pacman = pacman {queuedDir = Just Model.Right}
bufferInput _ pacman = pacman

updatePacManPosition :: Grid -> PacMan -> PacMan
updatePacManPosition _ pacman = pacman