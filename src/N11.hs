module N11 where

import qualified Data.HashSet as HashSet
import Data.List (sort)

printBoard :: [[Char]] -> String
printBoard b =
  foldl (\acc s -> acc ++ "\n" ++ s) "" $ b

getXysWithoutStars :: [[Char]] -> ([Int],[Int])
getXysWithoutStars b =
  let xys :: [(Int, Int)]
      xys = [(x, y) |
              (y,line) <- zip [0..] b,
              (x,'#') <- zip [0..] line]
      xs = HashSet.fromList [x | (x,_) <- xys]
      ys = HashSet.fromList [y | (_,y) <- xys]
      xsWithoutStars = HashSet.difference
        (HashSet.fromList [0..((length b) - 1)])
        xs
      ysWithoutStars = HashSet.difference
        (HashSet.fromList [0..((length b) - 1)])
        ys
  in (sort $ HashSet.toList xsWithoutStars,
      sort $ HashSet.toList ysWithoutStars)

countWithoutStarsSameAxis :: [Int] -> Int -> Int -> Int
countWithoutStarsSameAxis axisNoStars a0 a1 =
  let smallA = min a0 a1
      bigA = max a0 a1
  in length [a | a <- axisNoStars, a > smallA && a < bigA]

pairDistance :: Int -> ([Int],[Int]) -> ((Int, Int), (Int, Int)) -> Int
pairDistance extraStars (xsNoStars, ysNoStars) ((x0, y0), (x1, y1)) =
  let xsWithoutStars = countWithoutStarsSameAxis xsNoStars x0 x1
      ysWithoutStars = countWithoutStarsSameAxis ysNoStars y0 y1
  in (abs $ y0 - y1) + (extraStars * ysWithoutStars)
     + (abs $ x0 - x1) + (extraStars * xsWithoutStars)

starPositions :: [[Char]] -> [(Int, Int)]
starPositions b =
  [(x,y) | (y, line) <- zip [0..] b, (x, '#') <- zip [0..] line]

makePairs :: [((Int, Int), (Int, Int))] -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
makePairs acc (f : rest) =
  makePairs ([(f, s) | s <- rest] ++ acc) rest

makePairs acc _ = acc

getAnswer :: Int -> [[Char]] -> Int
getAnswer inc b =
  let xysNoStars = getXysWithoutStars b
  in sum $ [pairDistance inc xysNoStars pair | pair <- makePairs [] . starPositions $ b]

main :: IO()
main = do
  fileContent <- readFile "inputs/11_p1.txt"
  let originalBoard = lines fileContent
  print $ getAnswer 1 originalBoard
  print $ getAnswer 999999 originalBoard
