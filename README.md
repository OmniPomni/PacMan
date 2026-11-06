# PacMan

Pac-Man written in haskell using Gloss.

From the folder containing 'pacman.cabal' run:

```sh
cabal build
cabal run pacman
```
## Controls
| Key | Action |
| --- | --- |
| Enter | Open the level menu from the title screen, or return to the title screen after a run |
| 1, 2, 3 | Select a level from the level menu |
| Arrow keys | Move Pac-Man |
| P | Pause or resume |

## How to play

Collect every pellet and energizer to clear the maze. Energizers temporarily frigthen  the ghosts turning them blue, allowing Pac-Man to eat them for bonus points. 
Colliding with a ghost when they are not frightened loses you a life. 
The game ends when you have no more lives.
Your highscores are stored in highscore.txt
