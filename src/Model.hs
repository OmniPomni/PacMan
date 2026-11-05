module Model where

import System.Random (StdGen, getStdGen)

type Position = (Int, Int)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Eq, Show)

data Tile
  = Wall
  | Pellet
  | Energizer
  | Empty
  | Gate
  deriving (Eq, Show)

type Grid = [[Tile]]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

setTile :: Position -> Tile -> Grid -> Grid
setTile (x, y) newTile g = replaceAt y (replaceAt x newTile (g !! y)) g

data PacMan = PacMan
  { pacmanPos :: Position
  , pacmanDir :: Direction
  , queuedDir :: Maybe Direction
  } deriving (Show)

data GhostType
  = Red
  | Pink
  | Cyan
  | Orange
  | Purple
  deriving (Eq, Show)

data GhostMode
  = Chase
  | Scatter
  | Frightened
  deriving (Eq, Show)

data Ghost = Ghost
  { ghostType      :: GhostType
  , ghostPos       :: Position
  , ghostDir       :: Direction
  , ghostMode      :: GhostMode
  , homeCorner     :: Position
  , frightenedTime :: Float
  } deriving (Show)

data GameScreen
  = StartScreen
  | LevelSelect
  | Playing
  | GameOver
  deriving (Eq, Show)

data Level = Level
  { levelNumber :: Int
  , levelGrid   :: Grid
  , ghostSpeed  :: Float
  , ghostRoster :: [GhostType]
  } deriving (Show)

data GameState = GameState
  { player         :: PacMan
  , ghosts         :: [Ghost]
  , grid           :: Grid
  , score          :: Int
  , lives          :: Int
  , currentMode    :: GhostMode
  , modeTimer      :: Float
  , isPaused       :: Bool
  , rng            :: StdGen
  , animationTimer :: Float
  , deathAnimation :: Maybe Float
  , highScore      :: Int
  , currentLevel   :: Int
  , gameScreen     :: GameScreen
  } deriving (Show)

class Movable a where
  getPos :: a -> Position
  getDir :: a -> Direction
  setPos :: Position -> a -> a

instance Movable PacMan where
  getPos = pacmanPos
  getDir = pacmanDir
  setPos pos p = p { pacmanPos = pos }

instance Movable Ghost where
  getPos = ghostPos
  getDir = ghostDir
  setPos pos g = g { ghostPos = pos }

stepInDir :: Position -> Direction -> Position
stepInDir (x, y) dir = case dir of
  Model.Up    -> (x, y - 1)
  Model.Down  -> (x, y + 1)
  Model.Left  -> (x - 1, y)
  Model.Right -> (x + 1, y)

-- Prevents crashing by returning Wall for out-of-grid positions.
getTile :: Grid -> Position -> Tile
getTile g (x, y)
  | y < 0 || y >= length g                  = Wall
  | x < 0 || null g || x >= length (g !! y) = Wall
  | otherwise                               = (g !! y) !! x

-- Default initial game state for testing
initialGame :: IO GameState
initialGame = do
  gen <- getStdGen
  
  let layout = 
        [ "############################"
        , "#............##............#"
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
        , "#..........................#"
        , "############################"
        ]
        
      charToTile '#' = Wall
      charToTile '-' = Gate
      charToTile '.' = Pellet
      charToTile _   = Empty
      
      arcadeGrid = map (map charToTile) layout
                 
  return GameState
    { player         = PacMan (13, 23) Model.Down Nothing
    , ghosts         = [Ghost Red (13, 11) Model.Left Chase (26, 1) 0.0, Ghost Pink (13, 14) Model.Left Chase (1, 1) 0.0, Ghost Cyan (12, 14) Model.Left Chase (26, 1) 0.0, Ghost Orange (14, 14) Model.Left Chase (1, 1) 0.0]
    , grid           = arcadeGrid
    , score          = 0
    , lives          = 3
    , currentMode    = Scatter
    , modeTimer      = 0
    , isPaused       = False
    , rng            = gen
    , animationTimer = 0
    , deathAnimation = Nothing
    , highScore      = 0
    , currentLevel   = 1
    , gameScreen     = StartScreen
    }