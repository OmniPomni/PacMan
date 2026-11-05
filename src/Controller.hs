module Controller where

import Graphics.Gloss.Interface.IO.Game (Event)
import Model (GameState(..))
import Controller.Player ()
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
        EventKey (Char '1') Down __ -> gstate {gameScreen = Playing, currentLevel = 1}
        EventKey (Char '2') Down __ -> gstate {gameScreen = Playing, currentLevel = 2}
        EventKey (Char '3') Down __ -> gstate {gameScreen = Playing, currentLevel = 3}
        _                           -> gstate

    -- Press p to (un)pause the game and handle player input when the game is not paused
    Playing -> case event of
        EventKey (char 'p') Down _ _ -> gstate {isPaused = not (isPaused gstate)}
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
updateGame = undefined