module Levels where

import Model (GameState, Level, Grid)

loadHighScore :: FilePath -> IO Int
loadHighScore = undefined

saveHighScore :: FilePath -> Int -> IO ()
saveHighScore = undefined

loadLevel :: Level -> GameState -> GameState
loadLevel = undefined

saveGame :: FilePath -> GameState -> IO ()
saveGame = undefined

loadGame :: FilePath -> IO (Either String GameState)
loadGame = undefined

parseGrid :: String -> Grid
parseGrid = undefined

loadCustomLevel :: FilePath -> IO Grid
loadCustomLevel = undefined