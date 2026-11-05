module Controller where

import Graphics.Gloss.Interface.IO.Game
import Model (GameState(..), GameScreen(..))
import Controller.Player (bufferInput, updatePacManPosition)
import Controller.Ghost ()

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
        _                           -> gstate

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


updateGameIO :: Float -> GameState -> IO GameState
updateGameIO dt gstate = return (updateGame dt gstate)

updateGame :: Float -> GameState -> GameState
updateGame dt gstate
  | isPaused gstate = gstate
  | gameScreen gstate == Playing = 
      let newTimer = animationTimer gstate + dt
      in if newTimer >= 0.15 -- Game speed variable. Higher = slower, lower = faster
         then gstate { player = updatePacManPosition (grid gstate) (player gstate)
                     , animationTimer = 0 
                     }
         else gstate { animationTimer = newTimer }
  | otherwise = gstate