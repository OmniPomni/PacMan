module View where

import Graphics.Gloss (Picture, color, white, scale, translate, text)
import Model (GameState)

renderGameIO :: GameState -> IO Picture
renderGameIO gstate = return (renderGame gstate)

renderGame :: GameState -> Picture
renderGame = undefined