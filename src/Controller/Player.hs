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
updatePacManPosition grid pacman = 
    let pos  = pacmanPos pacman
        cdir = pacmanDir pacman

        -- Helper to check if a direction is free of Walls and the Ghost Gate
        canMoveInDir dir = 
            let tile = Model.getTile grid (Model.stepInDir pos dir)
            in tile /= Wall && tole /= Gate

    in case queuedDir pacman of
        -- If the queued direction is valid, move in that direction and clear the queue
        Just qDir | canMove qDir ->
            pacman { pacmanPos = Model.stepInDir pos qDir
                   , pacmanDir = qDir
                   , queuedDir = Nothing }
        -- if the current path is clear, continue moving in current direction
        _ | canMove cDir ->
            pacman { pacmanPos = Model.stepInDir pos cDir }
        -- if both the queued direction and current direction are blocked, stop moving
        _ -> pacman