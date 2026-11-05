module View where

import Graphics.Gloss
import Model (GameState(..), GameScreen(..), Grid, Tile(..), PacMan(..), Position, Ghost(..), GhostType(..))

tileSize :: Float
tileSize = 24.0

-- Helper function to convert grid coordinates to screen coordinates
gridToScreen :: Position -> (Float, Float)
gridToScreen (x, y) = 
  let gridWidth  = 28.0 -- Total columns in the arcade maze
      gridHeight = 31.0 -- Total rows in the arcade maze
      
      -- Calculate the starting top-left corner of the grid
      offsetX = - (gridWidth * tileSize) / 2
      offsetY =   (gridHeight * tileSize) / 2
      
      -- Translate grid (x, y) to window coordinates
      -- We subtract the Y value because grid rows go down, but Gloss Y goes up
      -- We add (tileSize / 2) to center the graphics perfectly inside the tile
      screenX = offsetX + (fromIntegral x * tileSize) + (tileSize / 2)
      screenY = offsetY - (fromIntegral y * tileSize) - (tileSize / 2)
      
  in (screenX, screenY)

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

-- Draws the grid by iterating over each tile and drawing it
drawGrid :: Grid -> Picture
drawGrid g = pictures 
  [ let (sx, sy) = gridToScreen (x, y)
    in translate sx sy (drawTile tile)
  | (y, row) <- zip [0..] g
  , (x, tile) <- zip [0..] row
  ]

drawTile :: Tile -> Picture
drawTile Wall = color blue $ rectangleSolid tileSize tileSize
drawTile Gate = color white $ rectangleSolid tileSize (tileSize / 4)
drawTile _    = blank

renderPlaying :: GameState -> Picture
renderPlaying gstate = pictures $ 
  [ drawGrid (grid gstate)
  , drawPacMan (player gstate)
  ] ++ map drawGhost (ghosts gstate) 
    ++ [ drawHUD (lives gstate) ]

drawGhost :: Ghost -> Picture
drawGhost g =
  let (sx, sy) = gridToScreen (ghostPos g)
      gColor   = case ghostType g of
                   Red    -> red
                   Pink   -> magenta
                   Cyan   -> cyan
                   Orange -> orange
                   Purple -> violet
  in translate sx sy $ color gColor $ circleSolid (tileSize / 2)

drawHUD :: Int -> Picture
drawHUD livesCount =
  -- Draw the lives count at the top left corner of the screen
  translate 0 (-tileSize) $ scale 0.15 0.15 $ color white $ text ("Lives: " ++ show livesCount)

renderGameOver :: Picture
renderGameOver = pictures 
  [ translate (-150) 20 $ scale 0.25 0.25 $ color red $ text "GAME OVER"
  , translate (-200) (-30) $ scale 0.15 0.15 $ color white $ text "Press ENTER to return"
  ]

-- Draws Pac-Man at his current position
drawPacMan :: PacMan -> Picture
drawPacMan pacman = 
    let (sx, sy) = gridToScreen (pacmanPos pacman)
    in translate sx sy $ color yellow $ circleSolid (tileSize / 2)