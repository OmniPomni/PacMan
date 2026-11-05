module View where

import Graphics.Gloss
import Model (GameState(..), GameScreen(..), Grid, Tile(..), PacMan(..), Position)

tileSize :: Float
tileSize = 40.0

-- Helper function to convert grid coordinates to screen coordinates
gridToScreen :: Position -> (Float, Float)
gridToScreen (x, y) = (fromIntegral x * tileSize, fromIntegral y * tileSize)

-- IO wrapper for rendering game state
renderGameIO :: GameState -> IO Picture
renderGameIO gstate = return (renderGame gstate)

-- actual rendering function that turns game state into a picture
renderGame :: GameState -> Picture
renderGame gstate = case gameScreen gstate of
    StartScreen -> renderStartScreen
    LevelSelect -> renderLevelSelect
    Playing     -> renderPlaying gstate
    GameOver    -> renderGameOver

-- renderGame helper functions
renderStartScreen :: Picture
renderStartScreen = translate (-150) 0 $ scale 0.25 0.25 $ color white $ text "Press ENTER to Start"

renderLevelSelect :: Picture
renderLevelSelect = translate (-200) 0 $ scale 0.25 0.25 $ color white $ text "Press 1 to load Test Maze"

renderPlaying :: GameState -> Picture
renderPlaying gstate = pictures [drawGrid (grid gstate), drawPacMan (player gstate)]

renderGameOver :: Picture
renderGameOver = translate (-150) 0 $ scale 0.25 0.25 $ color red $ text "GAME OVER" 

-- Draws the grid by iterating over each tile and drawing it
drawGrid :: Grid -> Picture
drawGrid g = pictures 
  [ translate (fromIntegral x * tileSize) (fromIntegral y * tileSize) (drawTile tile)
  | (y, row) <- zip [(0 :: Int)..] g
  , (x, tile) <- zip [(0 :: Int)..] row 
  ]
  where
    drawTile Wall = color blue $ rectangleSolid tileSize tileSize
    drawTile _    = blank -- Only draw walls for now; other tiles are blank

-- Draws Pac-Man at his current position
drawPacMan :: PacMan -> Picture
drawPacMan pacman = 
    let (sx, sy) = gridToScreen (pacmanPos pacman)
    in translate sx sy $ color yellow $ circleSolid (tileSize / 2)