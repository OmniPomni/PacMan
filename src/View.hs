module View where

import Graphics.Gloss
import Model (GameState(..), GameScreen(..), Grid, Tile(..), PacMan(..), Position, Ghost(..), GhostType(..), GhostMode(..))
import qualified Model

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
    Playing 
      | isPaused gstate -> pictures [renderPlaying gstate, renderPauseOverlay]
      | otherwise       -> renderPlaying gstate
    LevelComplete -> renderLevelComplete
    GameOver    -> renderGameOver

-- renderGame helper functions
renderStartScreen :: Picture
renderStartScreen = translate (-150) 0 $ scale 0.25 0.25 $ color white $ text "Press ENTER to Start"

renderLevelSelect :: Picture
renderLevelSelect = pictures
  [ translate (-125) 65 $ scale 0.22 0.22 $ color green $ text "SELECT LEVEL"
  , translate (-145) 15 $ scale 0.16 0.16 $ color white $ text "1  Easy"
  , translate (-145) (-20) $ scale 0.16 0.16 $ color white $ text "2  Medium"
  , translate (-145) (-55) $ scale 0.16 0.16 $ color white $ text "3  Good Luck :3"
  ]

renderLevelComplete :: Picture
renderLevelComplete = pictures 
  [ translate (-120) 20 $ scale 0.25 0.25 $ color green $ text "YOU WIN!"
  , translate (-200) (-30) $ scale 0.15 0.15 $ color white $ text "Press ENTER to return"
  ]

-- Draws the grid by iterating over each tile and drawing it
drawGrid :: Grid -> Picture
drawGrid g = pictures 
  [ let (sx, sy) = gridToScreen (x, y)
    in translate sx sy (drawTile tile)
  | (y, row) <- zip [0..] g
  , (x, tile) <- zip [0..] row
  ]

drawTile :: Tile -> Picture
drawTile Wall      = color blue $ rectangleSolid tileSize tileSize
drawTile Gate      = color white $ rectangleSolid tileSize (tileSize / 4)
drawTile Pellet    = color (light (light orange)) $ circleSolid (tileSize / 6)
drawTile Energizer = color chartreuse $ circleSolid (tileSize / 2.5)
drawTile _         = blank

renderPlaying :: GameState -> Picture
renderPlaying state = pictures
  [ drawGrid (grid state)
  , actors
  , drawHUD (lives state) (score state)
  ]
  where
    actors :: Picture
    actors = case deathAnimation state of
      Just elapsed -> drawPacMan (deathMouthAngle elapsed) (player state)
      Nothing ->
        pictures
          (drawPacMan (mouthAngle (mouthAnimationTime state)) (player state)
            : map drawGhost (ghosts state))

drawGhost :: Ghost -> Picture
drawGhost g =
  let (sx, sy) = gridToScreen (ghostPos g)
      gColor   = if ghostMode g == Frightened 
                 then azure 
                 else case ghostType g of
                        Red    -> red
                        Pink   -> magenta
                        Cyan   -> cyan
                        Orange -> orange
                        Purple -> violet
  in translate sx sy $ color gColor $ circleSolid (tileSize / 2)

drawHUD :: Int -> Int -> Picture
drawHUD livesCount currentScore = pictures
  [ -- Score positioned above the top-left of the maze
    translate (-300) 390 $ scale 0.15 0.15 $ color white $ text ("Score: " ++ show currentScore)
  , -- Lives positioned below the bottom-left of the maze
    translate (-300) (-390) $ scale 0.15 0.15 $ color white $ text ("Lives: " ++ show livesCount)
  ]

renderPauseOverlay :: Picture
renderPauseOverlay = pictures
  [color (makeColor 0 0 0 0.85) $ rectangleSolid 340 130
  , translate (-85) 15 $ scale 0.30 0.30 $ color white $ text "PAUSED"
  , translate (-105) (-30) $ scale 0.15 0.15 $ color white $ text "Press P to resume"
  ]

renderGameOver :: Picture
renderGameOver = pictures 
  [ translate (-150) 20 $ scale 0.25 0.25 $ color red $ text "GAME OVER"
  , translate (-200) (-30) $ scale 0.15 0.15 $ color white $ text "Press ENTER to return"
  ]

-- Draws Pac-Man at his current position
mouthFrameDuration :: Float
mouthFrameDuration = 0.04

mouthAngle :: Float -> Float
mouthAngle elapsed =
  case (floor (elapsed / mouthFrameDuration) :: Int) `mod` 4 of
    0 -> 0
    2 -> 40
    _ -> 20

deathMouthAngle :: Float -> Float
deathMouthAngle elapsed =
  let frameCount = 7 :: Int
      progress = elapsed / Model.deathAnimationDuration
      frame = min (frameCount - 1) (floor (progress * fromIntegral frameCount))
  in 180 * fromIntegral frame / fromIntegral (frameCount - 1)

drawPacMan :: Float -> PacMan -> Picture
drawPacMan opening pacman =
-- Keep color inside rotate so Gloss rotates the whole picture, not doing this distorts the animation.
 translate sx sy $ rotate rotation $ color yellow body
  where
    (sx, sy) = gridToScreen (pacmanPos pacman)
    radius = tileSize / 2

    rotation = case pacmanDir pacman of
      Model.Right -> 0
      Model.Down  -> 90
      Model.Left  -> 180
      Model.Up    -> -90

    body
      | opening <= 0   = circleSolid radius
      | opening >= 180 = blank
      | otherwise      = thickArc opening (360 - opening) (radius / 2) radius