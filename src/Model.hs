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
  Model.Up    -> (x, y + 1)
  Model.Down  -> (x, y - 1)
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
  
  let testGrid = 
        [ [Wall, Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall]
        , [Wall, Empty, Empty, Empty, Wall,  Empty, Empty, Empty, Empty, Empty, Wall]
        , [Wall, Empty, Wall,  Empty, Wall,  Empty, Wall,  Wall,  Wall,  Empty, Wall]
        , [Wall, Empty, Wall,  Empty, Empty, Empty, Empty, Empty, Wall,  Empty, Wall]
        , [Wall, Empty, Wall,  Wall,  Wall,  Empty, Wall,  Empty, Wall,  Empty, Wall]
        , [Wall, Empty, Empty, Empty, Empty, Empty, Wall,  Empty, Empty, Empty, Wall]
        , [Wall, Empty, Wall,  Wall,  Wall,  Empty, Wall,  Wall,  Wall,  Empty, Wall]
        , [Wall, Empty, Empty, Empty, Wall,  Empty, Empty, Empty, Empty, Empty, Wall]
        , [Wall, Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall] ]
                 
  return GameState
    { player         = PacMan (1, 1) Model.Right Nothing
    , ghosts         = []
    , grid           = testGrid
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