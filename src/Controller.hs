module Controller where

import Graphics.Gloss.Interface.IO.Game
import Model (GameState(..), GameScreen(..), PacMan(..), Tile(..), Ghost(..), GhostType(..), GhostMode(..), setTile, Grid)
import qualified Model
import Controller.Player (bufferInput, updatePacManPosition)
import Controller.Ghost (updateGhosts)

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
        EventKey (Char '1') Down _ _ -> gstate {gameScreen = Playing, currentLevel = 1}
        EventKey (Char '2') Down _ _ -> gstate {gameScreen = Playing, currentLevel = 2}
        EventKey (Char '3') Down _ _ -> gstate {gameScreen = Playing, currentLevel = 3}
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


handleCollisions :: GameState -> GameState
handleCollisions gstate
  -- Hit a regular ghost
  | any (\g -> ghostPos g == pPos && ghostMode g /= Frightened) gs =
      if lives gstate > 1
      then gstate { lives  = lives gstate - 1
                  , player = PacMan (13, 23) Model.Left Nothing
                  , ghosts = [Ghost Red (13, 11) Model.Left Chase (26, 1) 0.0] 
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
updateGame dt gstate
  | isPaused gstate = gstate
  | gameScreen gstate == Playing = 
      let gstateWithTimer = updateWaveTimer dt gstate
          newAnimTimer    = animationTimer gstateWithTimer + dt
      in if newAnimTimer >= 0.15 
         then let (newRng, updatedGhosts) = updateGhosts gstateWithTimer
                  movedState = gstateWithTimer { player = updatePacManPosition (grid gstateWithTimer) (player gstateWithTimer)
                                               , ghosts = updatedGhosts
                                               , rng    = newRng
                                               , animationTimer = 0 
                                               }
              in handleCollisions (consumeTile movedState)
         else gstateWithTimer { animationTimer = newAnimTimer }
  | otherwise = gstate
