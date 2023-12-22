module N09 (main) where

import Data.List (foldl')
import Data.List.Split (splitOn)

getRateOfIncrease :: [Int] -> [Int]
getRateOfIncrease (f : s : xs) =
  [(f - s)] ++ getRateOfIncrease ([s] ++ xs)
getRateOfIncrease (_ : []) =
  []
getRateOfIncrease [] =
  []

getRatesUntilAllZero :: [[Int]] -> [[Int]]
getRatesUntilAllZero (f : xs) =
  if all (== 0) f then
    [f] ++ xs
  else
    getRatesUntilAllZero $ [(getRateOfIncrease f)] ++ [f] ++ xs
getRatesUntilAllZero [] =
  []

getNextInList :: [Int] -> Int
getNextInList l =
  let reversed = reverse l
  in foldl' (\acc l -> acc + (l !! 0)) 0 $ getRatesUntilAllZero [reversed]

getNextInListNoReverse :: [Int] -> Int
getNextInListNoReverse l = foldl' (\acc l -> acc + (l !! 0)) 0 $ getRatesUntilAllZero [l]

parseLists :: String -> [[Int]]
parseLists s = map (\line ->
                      map read (splitOn " " line)) $ lines s

main :: IO()
main = do
  fileContent <- readFile "inputs/09_p1.txt"
  let inputs = parseLists fileContent
      nexts = map getNextInList inputs
      nextsP2 = map getNextInListNoReverse inputs
  print $ sum nexts
  print $ sum nextsP2
