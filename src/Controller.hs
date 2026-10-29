module Controller where

import Graphics.Gloss.Interface.IO.Game (Event)
import Model (GameState(..))
import Controller.Player ()
import Controller.Ghost ()

handleInputIO :: Event -> GameState -> IO GameState
handleInputIO event gstate = return (handleInput event gstate)

handleInput :: Event -> GameState -> GameState
handleInput = undefined

updateGameIO :: Float -> GameState -> IO GameState
updateGameIO dt gstate = return (updateGame dt gstate)

updateGame :: Float -> GameState -> GameState
updateGame = undefined