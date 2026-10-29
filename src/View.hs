module View where

import Graphics.Gloss (Picture)
import Model (GameState)

renderGameIO :: GameState -> IO Picture
renderGameIO gstate = return (renderGame gstate)

renderGame :: GameState -> Picture
renderGame = undefined