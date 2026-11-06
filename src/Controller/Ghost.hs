module Controller.Ghost (updateGhosts) where

import System.Random (randomR, StdGen)
import Data.List (minimumBy, mapAccumL)
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
  Model.Up    -> (x, y - 4)
  Model.Down  -> (x, y + 4)
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
targetPurple purple gamestate
  | pacmanIsNear gamestate = targetRed pPos
  | redIsClose gamestate   = targetCyan pPos pDir (getRedPos gamestate)
  | otherwise              = targetPink pPos pDir
  where
    pPos = pacmanPos (player gamestate)
    pDir = pacmanDir (player gamestate)

    pacmanIsNear :: GameState -> Bool
    pacmanIsNear state = 
     distance (ghostPos purple) (pacmanPos (player state)) <= 3

    redIsClose :: GameState -> Bool
    redIsClose state =
      case filter ((== Red) . ghostType) (ghosts state) of
        (red: _) -> distance (ghostPos red) (pacmanPos (player state)) <= 4
        []       -> False 


distance :: Position -> Position -> Float
distance (x1, y1) (x2, y2) =
  sqrt (fromIntegral ((x2 - x1) ^(2 :: Int) + (y2 - y1) ^ (2 :: Int)))


targetAhead :: Int -> Position -> Direction -> Position
targetAhead n (x, y) dir = case dir of
  Model.Up    -> (x, y - n)
  Model.Down  -> (x, y + n)
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

-- Guide ghosts through the shared cage gate after they spawn.
houseExitDirection :: Grid -> Position -> Maybe Direction
houseExitDirection maze pos@(x, y)
  | insideHouse || onGate
  , getTile maze (exitColumn, gateRow) == Model.Gate
  , getTile maze (stepInDir pos direction) /= Wall = Just direction
  | otherwise = Nothing
  where
    (leftGate, gateRow) = (13, 12)
    rightGate = leftGate + 1
    (houseLeft, houseTop) = (11, 13)
    (houseRight, houseBottom) = (16, 15)

    insideHouse =
      x >= houseLeft && x <= houseRight && y >= houseTop && y <= houseBottom
    onGate = y == gateRow && (x == leftGate || x == rightGate)
    exitColumn = if x <= leftGate then leftGate else rightGate

    direction
      | x < exitColumn = Model.Right
      | x > exitColumn = Model.Left
      | otherwise      = Model.Up

getValidGhostDirs :: Grid -> Ghost -> [Direction]
getValidGhostDirs maze ghost =
  let allDirs = [Model.Up, Model.Down, Model.Left, Model.Right]

      canMove :: Direction -> Bool
      canMove dir =
        let tile = getTile maze (stepInDir (ghostPos ghost) dir)
        in tile /= Wall && tile /= Model.Gate

      validDirs = filter canMove allDirs
      forwardDirs = filter (/= oppositeDir (ghostDir ghost)) validDirs
  in if null forwardDirs then validDirs else forwardDirs

updateGhosts :: GameState -> (StdGen, [Ghost])
updateGhosts gstate = mapAccumL (updateSingleGhost gstate) (rng gstate) (ghosts gstate)

updateSingleGhost :: GameState -> StdGen -> Ghost -> (StdGen, Ghost)
updateSingleGhost gstate gen ghost
  | Just dir <- houseExitDirection maze (ghostPos ghost) = (gen, move dir)
  | otherwise =
      case getValidGhostDirs maze ghost of
        [] -> (gen, ghost)
        validDirs
          | ghostMode ghost == Frightened ->
              let (index, nextGen) = randomR (0, length validDirs - 1) gen
              in (nextGen, move (validDirs !! index))
          | otherwise ->
              let target = ghostTarget ghost gstate
                  dir = chooseNextMove (ghostPos ghost) validDirs target
              in (gen, move dir)
  where
    maze = grid gstate

    move :: Direction -> Ghost
    move dir =
      ghost { ghostPos = stepInDir (ghostPos ghost) dir, ghostDir = dir }