module Controller where

import Graphics.Gloss.Interface.IO.Game
import Model (GameState(..), GameScreen(..), PacMan(..), Ghost(..), GhostType(..), GhostMode(..), setTile)
import qualified Model
import Controller.Player (bufferInput, updatePacManPosition)
import Controller.Ghost (updateGhost)

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


handleCollisions :: GameState -> GameState
handleCollisions gstate
    | any (\g -> ghostPos g == pacmanPos (player gstate)) (ghosts gstate) =
        if lives gstate > 1
        then gstate { lives = lives gstate - 1
                    , player = PacMan (13, 23) Model.Left Nothing
                    , ghosts = [Ghost Red (13, 11) Model.Left Chase (26, 1) 0.0, Ghost Pink (13, 14) Model.Left Chase (1, 1) 0.0, Ghost Cyan (12, 14) Model.Left Chase (26, 1) 0.0, Ghost Orange (14, 14) Model.Left Chase (1, 1) 0.0] }
        else gstate { lives = 1
                    , gameScreen = GameOver }
    | otherwise = gstate


updateWaveTimer :: Float -> GameState -> GameState
updateWaveTimer dt gstate = 
    let newTimer = modeTimer gstate + dt
        cMode = currentMode gstate

        (nextMode, threshold) = case cMode of
            Scatter    -> (Chase, 7.0)
            Chase      -> (Scatter, 20.0)
            _          -> (cMode, 999.0)  -- Fallback for Frightened mode, which doesn't switch automatically
    
    in if newTimer >= threshold
        then gstate { currentMode = nextMode
                    , modeTimer   = 0
                    , ghosts      = map (\g -> g {ghostMode = nextMode}) (ghosts gstate) }
        else gstate { modeTimer = newTimer }

consumeTile :: GameState -> GameState
consumeTile gstate =
    let pPos = pacmanPos (player gstate)
        g    = grid gstate
    in case Model.getTile g pPos of
        Model.Pellet -> gstate { grid  = Model.setTile pPos Model.Empty g
                               , score = score gstate + 10 }
        _            -> gstate

updateGameIO :: Float -> GameState -> IO GameState
updateGameIO dt gstate = return (updateGame dt gstate)

updateGame :: Float -> GameState -> GameState
updateGame dt gstate
  | isPaused gstate = gstate
  | gameScreen gstate == Playing = 
      let gstateWithTimer = updateWaveTimer dt gstate
          newAnimTimer    = animationTimer gstateWithTimer + dt
      in if newAnimTimer >= 0.15 
         then let movedState = gstateWithTimer { player = updatePacManPosition (grid gstateWithTimer) (player gstateWithTimer)
                                               , ghosts = map (updateGhost gstateWithTimer) (ghosts gstateWithTimer)
                                               , animationTimer = 0 
                                               }
              in handleCollisions (consumeTile movedState)
         else gstateWithTimer { animationTimer = newAnimTimer }
  | otherwise = gstate
