module Controller where

import Graphics.Gloss.Interface.IO.Game
import Model (GameState(..), GameScreen(..), PacMan(..), Tile(..), Ghost(..), GhostMode(..), setTile, Grid, Level(levelNumber),ghostsForLevel)
import qualified Model
import Controller.Player (bufferInput, updatePacManPosition)
import Controller.Ghost (updateGhosts)
import Data.List (find)
import Levels (defaultLevels, loadLevel)

handleInputIO :: Event -> GameState -> IO GameState
handleInputIO event gstate = return (handleInput event gstate)

handleInput :: Event -> GameState -> GameState
handleInput event gstate = case gameScreen gstate of

    -- Press Enter in main menu to go to level select
    StartScreen -> case event of 
        EventKey (SpecialKey KeyEnter) Down _ _ -> gstate {gameScreen = LevelSelect}
        _                                       -> gstate

    -- Press 1, 2, or 3 to select a level
    LevelSelect -> case event of
        EventKey (Char '1') Down _ _ -> startLevel 1 gstate
        EventKey (Char '2') Down _ _ -> startLevel 2 gstate
        EventKey (Char '3') Down _ _ -> startLevel 3 gstate
        _                            -> gstate

    -- Press p to (un)pause the game and handle player input when the game is not paused
    Playing -> case event of
        EventKey (Char 'p') Down _ _ -> gstate {isPaused = not (isPaused gstate)}
        _                          
          | isPaused gstate -> gstate
          | Just _ <- deathAnimation gstate -> gstate
          | otherwise -> gstate { player = bufferInput event (player gstate)}
    
    -- When dead, press Enter to return to the main menu
    GameOver -> case event of
        EventKey (SpecialKey KeyEnter) Down _ _ -> gstate {gameScreen = StartScreen}
        _                                       -> gstate

    -- When level is complete, press Enter to return to the main menu
    LevelComplete -> case event of
        EventKey (SpecialKey KeyEnter) Down _ _ -> gstate { gameScreen = StartScreen }
        _                                       -> gstate

startLevel :: Int -> GameState -> GameState
startLevel number state =
  case find ((== number) . levelNumber) defaultLevels of
    Just level -> loadLevel level state
    Nothing    -> state

handleCollisions :: GameState -> GameState -> GameState
handleCollisions before after
  | any ((/= Frightened) . ghostMode . snd) hits =
      after { deathAnimation = Just 0, gameScreen = Playing }
  | null hits = after
  | otherwise =
      after { score = score after + 500
            , ghosts = map resetIfHit ghostPairs
            }
  where
    oldPacManPos = pacmanPos (player before)
    newPacManPos = pacmanPos (player after)
    ghostPairs = zip (ghosts before) (ghosts after)
    hits = filter collided ghostPairs
-- A collision occurs when Pac-Man and a ghost end on the same tile
-- we check both the old and new positions, otherwise collision would only work if both pacman
-- and ghost are on the same tile on the same tick.
    collided :: (Ghost, Ghost) -> Bool
    collided (oldGhost, newGhost) =
      ghostPos newGhost == newPacManPos
        || (ghostPos oldGhost == newPacManPos
            && ghostPos newGhost == oldPacManPos)

    resetIfHit :: (Ghost, Ghost) -> Ghost
    resetIfHit pair@(_, ghost)
      | collided pair = ghost { ghostPos = (13, 11), ghostMode = Chase }
      | otherwise     = ghost


updateWaveTimer :: Float -> GameState -> GameState
updateWaveTimer dt gstate = 
    let newTimer = modeTimer gstate + dt
        cMode = currentMode gstate

        (nextMode, threshold) = case cMode of
            Scatter    -> (Chase, 7.0)
            Chase      -> (Scatter, 20.0)
            Frightened -> (Chase, 6.0)
    
    in if newTimer >= threshold
        then gstate { currentMode = nextMode
                    , modeTimer   = 0
                    , ghosts      = map (\g -> g {ghostMode = nextMode}) (ghosts gstate) }
        else gstate { modeTimer = newTimer }

isGridCleared :: Grid -> Bool
isGridCleared g = not $ any (any (\tile -> tile == Pellet || tile == Energizer)) g

consumeTile :: GameState -> GameState
consumeTile gstate =
  let pPos = pacmanPos (player gstate)
      g    = grid gstate
      newState = case Model.getTile g pPos of
                   Pellet    -> gstate { grid  = setTile pPos Empty g
                                       , score = score gstate + 10 
                                       }
                   Energizer -> gstate { grid        = setTile pPos Empty g
                                       , score       = score gstate + 50
                                       , currentMode = Frightened
                                       , modeTimer   = 0
                                       , ghosts      = map (\ghost -> ghost { ghostMode = Frightened }) (ghosts gstate)
                                       }
                   _         -> gstate
                   
  in if isGridCleared (grid newState)
     then newState { gameScreen = LevelComplete }
     else newState

advanceDeath :: Float -> Float -> GameState -> GameState
advanceDeath dt elapsed state
  | elapsed + dt < Model.deathAnimationDuration = 
      state { deathAnimation = Just (elapsed + dt) }
  | lives state <= 1 = state { lives = 0, deathAnimation = Nothing, gameScreen = GameOver}
  | otherwise = 
      state 
        { lives = lives state - 1
        , player = PacMan (13, 23) Model.Left Nothing
        , ghosts = ghostsForLevel (currentLevel state)
        , animationTimer = 0
        , ghostMoveTimer = 0
        , mouthAnimationTime = 0
        , deathAnimation = Nothing
        , gameScreen = if isGridCleared (grid state) then LevelComplete else Playing}


updateGameIO :: Float -> GameState -> IO GameState
updateGameIO dt gstate = return (updateGame dt gstate)

updateGame :: Float -> GameState -> GameState
updateGame dt state
  | isPaused state || gameScreen state /= Playing = state
-- Advance only the death animation while the board and ghost timers are frozen.
  | Just elapsed <- deathAnimation state = advanceDeath dt elapsed state
  | otherwise =
      let timed = updateWaveTimer dt state
          nextPlayer = updatePacManPosition (grid timed) (player timed)
          playerCanMove = pacmanPos nextPlayer /= pacmanPos (player timed)
          playerTime = animationTimer timed + dt
          ghostTime = ghostMoveTimer timed + dt
          playerInterval = 0.15
          ghostInterval = 1 / ghostMovesPerSecond timed
          playerReady = playerTime >= playerInterval
          ghostsReady = ghostMovesPerSecond timed > 0 && ghostTime >= ghostInterval

          (nextRng, nextGhosts) =
            if ghostsReady then updateGhosts timed else (rng timed, ghosts timed)

          moved = timed
            { player = if playerReady then nextPlayer else player timed
            , ghosts = nextGhosts
            , rng = nextRng
            , animationTimer = if playerReady then playerTime - playerInterval else playerTime
            , ghostMoveTimer = if ghostsReady then ghostTime - ghostInterval else ghostTime
            , mouthAnimationTime =
                if playerCanMove then mouthAnimationTime timed + dt else 0
            }

          afterTile = if playerReady then consumeTile moved else moved
            in if playerReady || ghostsReady
               then handleCollisions timed afterTile
               else afterTile
