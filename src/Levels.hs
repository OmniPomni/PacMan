module Levels where

import Model
  ( GameState(..), GameScreen(Playing), Ghost(ghostType), GhostMode(Scatter)
  , Grid, Level(..), PacMan(..), Tile(..)
  , ghostsForLevel, ghostsRoster
  )
import qualified Model
import Control.Exception (bracketOnError)
import qualified Data.ByteString.Char8 as BS
import System.Directory (removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (Handle, hClose, hPrint, hPutStrLn, openTempFile, stderr)
import System.IO.Error (isDoesNotExistError, tryIOError)
import Text.Read (readMaybe)

highScorePath :: FilePath
highScorePath = "highscore.txt"

loadHighScore :: FilePath -> IO Int
loadHighScore path = do
  result <- tryIOError (BS.readFile path)
  case result of
    Right contents ->
      case readMaybe (BS.unpack contents) :: Maybe Integer of
        Just value | value >= 0 && value <= toInteger (maxBound :: Int) ->
          pure (fromInteger value)
        _ -> pure 0
    Left err
      | isDoesNotExistError err -> pure 0
      | otherwise -> do
        hPutStrLn stderr ("Could not load" ++ show err)
        pure 0

saveHighScore :: FilePath -> Int -> IO ()
saveHighScore path value = bracketOnError
  (openTempFile (takeDirectory path) (takeFileName path ++ ".tmp"))
  cleanup
  (\(tempPath, handle)-> do
    hPrint handle value
    hClose handle
    --Replace old score after new file has been written
    renameFile tempPath path)
  where 
    cleanup :: (FilePath, Handle) -> IO ()
    cleanup (tempPath, handle) = do
      _ <- tryIOError (hClose handle)
      _ <- tryIOError (removeFile tempPath)
      pure ()

loadLevel :: Level -> GameState -> GameState
loadLevel level state =
  state
    { player              = PacMan (13, 23) Model.Down Nothing
    , ghosts              = ghostsRoster Scatter (ghostRoster level)
    , grid                = levelGrid level
    , score               = 0
    , lives               = 3
    , currentMode         = Scatter
    , modeTimer           = 0
    , isPaused            = False
    , animationTimer      = 0
    , ghostMoveTimer      = 0
    , ghostMovesPerSecond = ghostSpeed level
    , deathAnimation      = Nothing
    , currentLevel        = levelNumber level
    , gameScreen          = Playing
    }

gridFromLayout :: [String] -> Grid
gridFromLayout = map (map charToTile)
  where
    charToTile '#' = Wall
    charToTile '-' = Gate
    charToTile '.' = Pellet
    charToTile 'o' = Energizer
    charToTile _   = Empty

defaultLevels :: [Level]
defaultLevels =
  [ makeLevel 1 arcadeGrid (1 / 0.15)
  , makeLevel 2 level2Grid 7.5
  , makeLevel 3 level3Grid 8.5
  ]
  where
    makeLevel number maze speed =
      Level number maze speed (map ghostType (ghostsForLevel number))

-- Level 1
arcadeGrid :: Grid
arcadeGrid = gridFromLayout layout
  where
    layout =
      [ "############################"
      , "#o...........##...........o#"
      , "#.####.#####.##.#####.####.#"
      , "#.####.#####.##.#####.####.#"
      , "#.####.#####.##.#####.####.#"
      , "#..........................#"
      , "#.####.##.########.##.####.#"
      , "#.####.##.########.##.####.#"
      , "#......##....##....##......#"
      , "######.#####.##.#####.######"
      , "######.#####.##.#####.######"
      , "######.##..........##.######"
      , "######.##.###--###.##.######"
      , "######.##.#      #.##.######"
      , "..........#      #.........."
      , "######.##.#      #.##.######"
      , "######.##.########.##.######"
      , "######.##..........##.######"
      , "######.##.########.##.######"
      , "######.##.########.##.######"
      , "#............##............#"
      , "#.####.#####.##.#####.####.#"
      , "#.####.#####.##.#####.####.#"
      , "#...##................##...#"
      , "###.##.##.########.##.##.###"
      , "###.##.##.########.##.##.###"
      , "#......##....##....##......#"
      , "#.##########.##.##########.#"
      , "#.##########.##.##########.#"
      , "#o...........##...........o#"
      , "############################"
      ]

level2Grid :: Grid
level2Grid = gridFromLayout
  [ "############################"
  , "#..........................#"
  , "#.###.#######.########.###.#"
  , "#.###.#######.########.###.#"
  , "#o.........................#"
  , "#.###.###.###.####.###.###.#"
  , "#.###.###.###.####.###.###.#"
  , "#.###.###.###.####.###.###.#"
  , "#.........###.............o#"
  , "#.###.###.###.####.###.###.#"
  , "#.###.###.###.####.###.###.#"
  , "#.###..................###.#"
  , "#.###.###.###--###.###.###.#"
  , "#.###.###.#      #.###.###.#"
  , "#.###.###.#      #.###.###.#"
  , "#.###.###.#      #.###.###.#"
  , "#.###.###.########.###.###.#"
  , "#..........................#"
  , "#.###.###.####.###.###.###.#"
  , "#.###.###.####.###.###.###.#"
  , "#.###..........###.........#"
  , "#.###.###.####.###.###.###.#"
  , "#.###.###.####.###.###.###.#"
  , "#o.........................#"
  , "#.###.###.####.###.###.###.#"
  , "#.###.###.####.###.###.###.#"
  , "#.###..................###.#"
  , "#.###.########.#######.###.#"
  , "#.###.########.#######.###.#"
  , "#.........................o#"
  , "############################"
  ]

level3Grid :: Grid
level3Grid = gridFromLayout
  [ "############################"
  , "#o.........................#"
  , "#.#####.############.#####.#"
  , "#.#####.############.#####.#"
  , "#.##...................o##.#"
  , "#.##.#######.##.#######.##.#"
  , "#.##.#######.##.#######.##.#"
  , "#.##.##..............##.##.#"
  , "#.##.##.#####.######.##.##.#"
  , "#.##.##.#####.######.##.##.#"
  , "#.##.##.#####.######.##.##.#"
  , "#....##.#..........#.##....#"
  , "#.##.##.#.###--###.#.##.##.#"
  , "#.##....#.#      #.#....##.#"
  , "#.##.##.#.#      #.#.##.##.#"
  , "#.##.##...#      #...##.##.#"
  , "#.##.##.#.########.#.##.##.#"
  , "#.##....#.########.#....##.#"
  , "#.##.##.#.########.#.##.##.#"
  , "#....##.#..........#.##....#"
  , "#.##.##.######.#####.##.##.#"
  , "#.##.##.######.#####.##.##.#"
  , "#.##.##.######.#####.##.##.#"
  , "#.##.##..............##.##.#"
  , "#.##.####.########.####.##.#"
  , "#.##.####.########.####.##.#"
  , "#.##o...................##.#"
  , "#.##########.##.##########.#"
  , "#.##########.##.##########.#"
  , "#.........................o#"
  , "############################"
  ]

saveGame :: FilePath -> GameState -> IO ()
saveGame = undefined

loadGame :: FilePath -> IO (Either String GameState)
loadGame = undefined

parseGrid :: String -> Grid
parseGrid = undefined

loadCustomLevel :: FilePath -> IO Grid
loadCustomLevel = undefined