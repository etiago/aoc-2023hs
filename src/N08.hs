{-# LANGUAGE RecordWildCards   #-}

module N08 (main) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (foldl', foldl1')
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad
import Data.Void

type Parser = Parsec Void String

data Fork = Fork
  { source :: String,
    left   :: String,
    right  :: String } deriving (Show)

data Input = Input
  { stepPattern :: [Char]
  , forks       :: [Fork] } deriving (Show)

pStep :: Parser Char
pStep = do
  c <- (char 'L') <|> (char 'R')
  return c

pFork :: Parser Fork
pFork = do
  source <- some upperChar
  void (string " = ")
  void (char '(')
  left <- some upperChar
  void (string ", ")
  right <- some upperChar
  void (char ')')
  void (char '\n')
  return Fork {..}

pInput :: Parser Input
pInput = do
  stepPattern <- some pStep
  void (string "\n\n")
  forks <- some pFork
  return Input {..}

type Steps = [Char]
type Routing = HashMap.HashMap String (String, String)

navigate :: Routing -> (Int, String, [String], Steps) -> Int
navigate r (it, cur, visited, ss) =
  if cur == "ZZZ" then
    it
  else
    let nextRoutes = HashMap.lookup cur r
        nextPair = case nextRoutes of
                     (Just v) -> v
                     _ -> error "error 1"
        nextVal = case ss of
                    'L' : _ -> fst nextPair
                    'R' : _ -> snd nextPair
                    _ -> error "fail"

    in navigate r ((it + 1), nextVal, (nextVal : visited), (drop 1 ss))

getPathUntilRepeat :: Routing -> (Steps, String, (HashSet.HashSet String), [String]) -> (Steps, String, (HashSet.HashSet String), [String])
getPathUntilRepeat r (ss, next, seen, path) =
  let nextRoutes = HashMap.lookup next r
      nextPair = case nextRoutes of
                   (Just v) -> v
                   _ -> error "error 1"
      nextVal = case ss of
                  'L' : _ -> fst nextPair
                  'R' : _ -> snd nextPair
                  _ -> error "fail"
  in if endsInZ nextVal then
    (ss, nextVal, seen, path)
  else
    getPathUntilRepeat r ((drop 1 ss), nextVal, (HashSet.insert nextVal seen), (path ++ [nextVal]))

stepsToReachEnd :: Steps -> Routing -> Int --[String]
stepsToReachEnd steps routing = navigate routing (0, "AAA", [], steps)

endsInA :: String -> Bool
endsInA s = case s of
              [_, _, 'A'] -> True
              _ -> False

endsInZ :: String -> Bool
endsInZ s = case s of
              [_, _, 'Z'] -> True
              _ -> False

getCycleLength :: Steps -> Routing -> String -> Int
getCycleLength ss r start =
  let (_, _, _, path) = getPathUntilRepeat r (ss, start, HashSet.singleton start, [start])
  in length path

stepsP2 :: Steps -> Routing -> Int
stepsP2 steps routing =
  let startingNodes = filter endsInA $ HashMap.keys routing
      cycles = map (getCycleLength steps routing) startingNodes
  in foldl1' lcm cycles

insertForkInHashMap :: HashMap.HashMap String (String, String) -> Fork -> HashMap.HashMap String (String, String)
insertForkInHashMap hm Fork {source, left, right} =
  HashMap.insert source (left, right) hm

parseShim :: Input -> (Steps, Routing)
parseShim Input {stepPattern, forks} =
  (concat $ repeat stepPattern,
   foldl' insertForkInHashMap HashMap.empty forks)

main :: IO()
main = do
  fileContent <- readFile "inputs/08_p1.txt"
  let puzzleInput = runParser pInput "" fileContent
      input = case puzzleInput of
        Right input -> input
        Left _ -> error "failed"
      (steps, routing) = parseShim input
      stepCountP1 = stepsToReachEnd steps routing
      stepCountP2 = stepsP2 steps routing
  print stepCountP1
  print stepCountP2
