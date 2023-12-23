module N10 where

import Data.List (foldl')
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (catMaybes)

type Board = [[Char]]
type Delta = (Int, Int)
data Path = Path { curPos :: (Int, Int), stepsTaken :: Int, board :: Board }
instance Show Path where
  show (Path {curPos = (x, y), stepsTaken = steps, board = b}) =
    "Current Position: " ++ show (x, y) ++
    "\nSteps Taken: " ++ show steps ++
    "\nBoard:\n" ++ unlines (map showRow b)
    where
      showRow row = "|" ++ map showCell row ++ "|"
      showCell c = if c == '.' then ' ' else c

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (a, b) (c, d) = (a + c, b + d)

-- Tuples to move the current position by
-- a delta.
allowedDirectionDeltas :: [Delta]
allowedDirectionDeltas = [(0,1),(1,0),(0,-1),(-1,0)]

-- For each of the tuples above, define a list of chars
-- that are valid pipe continuations when moving in
-- that direction.
allowedDirectionChars :: [[Char]]
allowedDirectionChars = [['|', 'L', 'J']
                        ,['-', 'J', '7']
                        ,['|', '7', 'F']
                        ,['-', 'L', 'F']]

-- Get a position from the board if it's a valid
-- pipe continuation suitable to produce a new path.
getSinglePositionFromBoard :: Board -> ((Int, Int), [Char]) -> Maybe (Int, Int)
getSinglePositionFromBoard board ((posX, posY), allowedDirections)
  = case [elem c allowedDirections |
          (y,line) <- zip [0..] board,
          (x,c) <- zip [0..] line,
          x == posX && y == posY]
    of
      [True] -> Just (posX, posY)
      _ -> Nothing

-- Blank out a position on the board with a '.'
-- Used after we move to ensure we're never able to walk back.
blankPositionInBoard :: Board -> (Int, Int) -> Board
blankPositionInBoard b (posX, posY) =
  [if y == posY then
     take posX line ++ ['.'] ++ drop (posX + 1) line
   else
     line
  | (y,line) <- zip [0..] b]

-- For a given path, get a list of all possible immediate new
-- paths.
findPossiblePaths :: Path -> [Path]
findPossiblePaths Path {curPos, stepsTaken, board} =
  let positions = map (addTuples curPos) allowedDirectionDeltas
      positionsAllowed = catMaybes
        $ map (\(allowedDirChars, p) -> getSinglePositionFromBoard board (p, allowedDirChars))
        $ zip allowedDirectionChars positions
  in [Path newPos (stepsTaken + 1) (blankPositionInBoard board curPos) | newPos <- positionsAllowed]

-- For a given starting path, explore all paths until
-- no more movement is possible.
exploreAllPaths :: Path -> [Path]
exploreAllPaths p =
  [p] ++ case (findPossiblePaths p) of
         ps -> concatMap (\ip -> exploreAllPaths ip) ps

-- Group a list of paths by a position tuple to the
-- shortest steps to it.
groupPositionToShortest :: [Path] -> HashMap.HashMap (Int, Int) Int
groupPositionToShortest ps =
  foldl' groupByPos HashMap.empty ps
  where
    groupByPos hm Path {curPos, stepsTaken} =
      case HashMap.lookup curPos hm of
        Just prevSteps | stepsTaken < prevSteps -> HashMap.insert curPos stepsTaken hm
                       | otherwise -> hm
        _ -> HashMap.insert curPos stepsTaken hm

-- For the grouping above, get the maximum path seen
getLongest :: HashMap.HashMap (Int, Int) Int -> Int
getLongest = maximum . HashMap.elems

findStart :: Board -> (Int, Int)
findStart board =
  case [(x,y) |
        (y,line) <- zip [0..] board,
        (x,'S') <- zip [0..] line] of
    [s] -> s
    [] -> error "No start"
    _ -> error "Multiple starts"

main :: IO()
main = do
  fileContent <- readFile "inputs/10_p1.txt"
  let board = lines fileContent
      start = findStart board
      longest =
        getLongest
        $ groupPositionToShortest
        $ exploreAllPaths
        $ Path start 0
        $ board
  print longest
