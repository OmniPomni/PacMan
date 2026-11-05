module Controller.Player where

import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), KeyState(..), SpecialKey(..))
import Model (PacMan(..), Grid, Tile(Wall, Gate))
import qualified Model

bufferInput :: Event -> PacMan -> PacMan
bufferInput (EventKey (SpecialKey KeyUp) Down _ _) pacman    = pacman {queuedDir = Just Model.Up}
bufferInput (EventKey (SpecialKey KeyDown) Down _ _) pacman  = pacman {queuedDir = Just Model.Down}
bufferInput (EventKey (SpecialKey KeyLeft) Down _ _) pacman  = pacman {queuedDir = Just Model.Left}
bufferInput (EventKey (SpecialKey KeyRight) Down _ _) pacman = pacman {queuedDir = Just Model.Right}
bufferInput _ p                                              = p

updatePacManPosition :: Grid -> PacMan -> PacMan
updatePacManPosition grid pacman =
  let pos  = pacmanPos pacman
      cDir = pacmanDir pacman
      
      -- Helper to check if a direction is free of Walls and the Ghost Gate
      canMove dir = 
        let tile = Model.getTile grid (Model.stepInDir pos dir)
        in tile /= Wall && tile /= Gate

  in case queuedDir pacman of
    -- 1. If there's a queued turn and it's clear, take it and clear the queue
    Just qDir | canMove qDir -> 
        pacman { pacmanPos = Model.stepInDir pos qDir
               , pacmanDir = qDir
               , queuedDir = Nothing 
               }
               
    -- 2. Otherwise, if the current forward path is clear, keep moving straight
    _ | canMove cDir -> 
        pacman { pacmanPos = Model.stepInDir pos cDir }
        
    -- 3. If both the queued turn and current path are blocked, stop moving
    _ -> 
        pacman