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

-- Default initial game state for testing
initialGame :: IO GameState
initialGame = undefined