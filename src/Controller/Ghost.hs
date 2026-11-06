module Controller.Ghost where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Model (GameState(..), Ghost(..), GhostType(..), GhostMode(..), Position, Direction, Grid, PacMan(..), Tile(Wall), stepInDir, getTile)
import qualified Model

ghostTarget :: Ghost -> GameState -> Position
ghostTarget ghost gamestate = case ghostMode ghost of
  Scatter    -> homeCorner ghost
  Frightened -> ghostPos ghost
  Chase      -> case ghostType ghost of
    Red    -> targetRed (pacmanPos (player gamestate))
    Pink   -> targetPink (pacmanPos (player gamestate)) (pacmanDir (player gamestate))
    Cyan   -> targetCyan (pacmanPos (player gamestate)) (pacmanDir (player gamestate)) (getRedPos gamestate)
    Orange -> targetOrange (ghostPos ghost) (pacmanPos (player gamestate)) (homeCorner ghost)
    Purple -> targetPurple ghost gamestate

getRedPos :: GameState -> Position
getRedPos gamestate =
  case filter (\g -> ghostType g == Red) (ghosts gamestate) of
    (red:_) -> ghostPos red
    []      -> pacmanPos (player gamestate)

chooseNextMove :: Position -> [Direction] -> Position -> Direction
chooseNextMove currentPos validDirs target =
  minimumBy (comparing (distance target . stepInDir currentPos)) validDirs

targetRed :: Position -> Position
targetRed playerPos = playerPos

targetPink :: Position -> Direction -> Position
targetPink (x, y) dir = case dir of
  Model.Up    -> (x, y + 4)
  Model.Down  -> (x, y - 4)
  Model.Left  -> (x - 4, y)
  Model.Right -> (x + 4, y)

targetCyan :: Position -> Direction -> Position -> Position
targetCyan playerPos pacmanDirection redPos =
  let intermediate = targetAhead 2 playerPos pacmanDirection
      vector       = intermediate `subtractPos` redPos
  in intermediate `addPos` vector

targetOrange :: Position -> Position -> Position -> Position
targetOrange orangePos playerPos orangeHomeCorner
  | distance orangePos playerPos > 8 = playerPos
  | otherwise                        = orangeHomeCorner

targetPurple :: Ghost -> GameState -> Position
targetPurple _ gamestate
  | pacmanIsNear gamestate = targetRed pPos
  | redIsClose gamestate   = targetCyan pPos pDir (getRedPos gamestate)
  | otherwise              = targetPink pPos pDir
  where
    pPos = pacmanPos (player gamestate)
    pDir = pacmanDir (player gamestate)

    pacmanIsNear :: GameState -> Bool
    pacmanIsNear = undefined

    redIsClose :: GameState -> Bool
    redIsClose = undefined


distance :: Position -> Position -> Float
distance (x1, y1) (x2, y2) =
  sqrt (fromIntegral ((x2 - x1) ^(2 :: Int) + (y2 - y1) ^ (2 :: Int)))


targetAhead :: Int -> Position -> Direction -> Position
targetAhead n (x, y) dir = case dir of
  Model.Up    -> (x, y + n)
  Model.Down  -> (x, y - n)
  Model.Left  -> (x - n, y)
  Model.Right -> (x + n, y)

addPos :: Position -> Position -> Position
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subtractPos :: Position -> Position -> Position
subtractPos (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

oppositeDir :: Direction -> Direction
oppositeDir dir = case dir of
  Model.Up    -> Model.Down
  Model.Down  -> Model.Up
  Model.Left  -> Model.Right
  Model.Right -> Model.Left


getValidGhostDirs :: Grid -> Ghost -> [Direction]
getValidGhostDirs g ghost = 
  let currentPos = ghostPos ghost
      currentDir = ghostDir ghost
      allDirs = [Model.Up, Model.Down, Model.Left, Model.Right]
      
      -- Check if current direction doesn't lead to a wall
      isNotWall d = getTile g (stepInDir currentPos d) /= Wall

      -- Filter out the reverse direction and walls but allow reversing if no other options are available
      validForwardDirs = filter (\d -> d /= oppositeDir currentDir && isNotWall d) allDirs
        in if null validForwardDirs
           then filter isNotWall allDirs -- If no valid forward direction, allow reversing
           else validForwardDirs