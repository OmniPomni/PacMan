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
        _                            -> 
            if not (isPaused gstate)
                then gstate { player = bufferInput event (player gstate)}
                else gstate
    
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

handleCollisions :: GameState -> GameState
handleCollisions gstate
  -- Hit a regular ghost
  | any (\g -> ghostPos g == pPos && ghostMode g /= Frightened) gs =
      if lives gstate > 1
      then gstate { lives  = lives gstate - 1
                  , player = PacMan (13, 23) Model.Left Nothing
                  , ghosts = ghostsForLevel (currentLevel gstate) 
                  }
      else gstate { lives = 0, gameScreen = GameOver }
      
  -- Hit a frightened ghost
  | any (\g -> ghostPos g == pPos && ghostMode g == Frightened) gs =
      let resetEaten g 
            | ghostPos g == pPos = g { ghostPos = (13, 11), ghostMode = Chase } 
            | otherwise          = g
      in gstate { score  = score gstate + 500
                , ghosts = map resetEaten gs 
                }
                
  -- No collision
  | otherwise = gstate
  where 
    pPos = pacmanPos (player gstate)
    gs   = ghosts gstate


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

updateGameIO :: Float -> GameState -> IO GameState
updateGameIO dt gstate = return (updateGame dt gstate)

updateGame :: Float -> GameState -> GameState
updateGame dt state
  | isPaused state || gameScreen state /= Playing = state
  | otherwise =
      let timed = updateWaveTimer dt state
          playerTime = animationTimer timed + dt
          ghostTime = ghostMoveTimer timed + dt
          playerInterval = 0.15
          ghostInterval = 1 / ghostMovesPerSecond timed
          playerReady = playerTime >= playerInterval
          ghostsReady = ghostMovesPerSecond timed > 0 && ghostTime >= ghostInterval

          (nextRng, nextGhosts) =
            if ghostsReady then updateGhosts timed else (rng timed, ghosts timed)

          moved = timed
            { player = if playerReady
                       then updatePacManPosition (grid timed) (player timed)
                       else player timed
            , ghosts = nextGhosts
            , rng = nextRng
            , animationTimer = if playerReady then playerTime - playerInterval else playerTime
            , ghostMoveTimer = if ghostsReady then ghostTime - ghostInterval else ghostTime
            }

          afterTile = if playerReady then consumeTile moved else moved
      in if playerReady || ghostsReady then handleCollisions afterTile else afterTile
