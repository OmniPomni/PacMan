module Controller.Ghost where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Model
    ( GameState(ghosts, player),
      Ghost(ghostPos, ghostMode, homeCorner, ghostType),
      GhostMode(Chase, Scatter, Frightened),
      GhostType(Red, Pink, Cyan, Orange, Purple),
      PacMan(pacmanDir, pacmanPos),
      Direction(..),
      Position )

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
distance = undefined

stepInDir :: Position -> Direction -> Position
stepInDir = undefined

targetAhead :: Int -> Position -> Direction -> Position
targetAhead = undefined

addPos :: Position -> Position -> Position
addPos = undefined

subtractPos :: Position -> Position -> Position
subtractPos = undefined