import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (elemIndex, sortBy, foldl', foldl1')
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, catMaybes, listToMaybe, mapMaybe)
import Debug.Trace (trace)

type Steps = [Char]
type Routing = HashMap.HashMap String (String, String)

getSteps :: [String] -> Steps
getSteps ls =
  case ls of
    (firstLine : _) -> concat $ repeat $ firstLine
    _ -> error "Expected many lines"

parseLeftRightStrs :: String -> String -> (String, String)
parseLeftRightStrs left right =
  let newLeft = drop 1 left
      newRight = take ((length right) - 1) right
  in (newLeft, newRight)

parseSingleRouting :: String -> HashMap.HashMap String (String, String) -> HashMap.HashMap String (String, String)
parseSingleRouting rl routing =
  let chunks = splitOn " = " rl
  in case chunks of
    [k, v] -> let leftRight = splitOn ", " v
              in case leftRight of
                   [left, right] -> HashMap.insert k (parseLeftRightStrs left right) routing
                   _ -> error "Don't have left and right"
    _ -> error "don't have a key and a value"

getRouting :: [String] -> Routing
getRouting ls =
  case ls of
    (firstLine : secondLine : routingLines) ->
      foldl' (\acc l -> (parseSingleRouting l acc)) HashMap.empty routingLines
    _ -> error "input error"

navigate :: Routing -> (Int, String, [String], Steps) -> (Int, String, [String], Steps)
navigate r (it, cur, visited, ss) =
  if cur == "ZZZ" then
    (it, cur, visited, ss)
  else
    let nextRoutes = HashMap.lookup cur r
        nextPair = case nextRoutes of
                     (Just v) -> v
                     _ -> error "error 1"
        nextVal = case ss of
                    'L' : _ -> fst nextPair
                    'R' : _ -> snd nextPair

    in navigate r ((it + 1), nextVal, (nextVal : visited), (drop 1 ss))

navigateBroken :: (Int, String, [String], Steps, Routing) -> (Int, String, [String], Steps, Routing)
navigateBroken (it, cur, visited, ss, r) =
  if cur == "ZZZ" then
    (it, cur, visited, ss, r)
  else
    let nextRoutes = HashMap.lookup cur r
        nextPair = case nextRoutes of
                     (Just v) -> v
                     _ -> error "error 1"
        nextVal = case ss of
                    'L' : _ -> fst nextPair
                    'R' : _ -> snd nextPair

    in navigateBroken ((it + 1), nextVal, (nextVal : visited), (drop 1 ss), r)

getPathUntilRepeat :: Routing -> (Steps, String, (HashSet.HashSet String), [String]) -> (Steps, String, (HashSet.HashSet String), [String])
getPathUntilRepeat r (ss, next, seen, path) =
  let nextRoutes = HashMap.lookup next r
      nextPair = case nextRoutes of
                   (Just v) -> v
                   _ -> error "error 1"
      nextVal = case ss of
                  'L' : _ -> fst nextPair
                  'R' : _ -> snd nextPair
  in if endsInZ nextVal then --HashSet.member nextVal seen && endsInZ nextVal then
    (ss, nextVal, seen, path)
  else
    getPathUntilRepeat r ((drop 1 ss), nextVal, (HashSet.insert nextVal seen), (path ++ [nextVal]))

stepsToReachEnd :: String -> Int --[String]
stepsToReachEnd l =
  let ls = lines l
      steps = getSteps ls
      routing = getRouting ls
      (it, cur, visited, ss) = navigate routing (0, "AAA", [], steps)
  in it

endsInA :: String -> Bool
endsInA s = case s of
              [_, _, 'A'] -> True
              _ -> False

endsInZ :: String -> Bool
endsInZ s = case s of
              [_, _, 'Z'] -> True
              _ -> False


getNextNode :: Routing -> Steps -> String -> String
getNextNode r ss cur =
  let nextRoutes = HashMap.lookup cur r
      nextPair = case nextRoutes of
                   (Just v) -> v
                   _ -> error "error 1"
  in case ss of
       'L' : _ -> fst nextPair
       'R' : _ -> snd nextPair

navigateMultiples :: Routing -> (Int, [String], Steps) -> (Int, [String], Steps)
navigateMultiples r (it, curs, ss) =
  if all endsInZ curs then
    (it, curs, ss)
  else
    let nextNodes = map (getNextNode r ss) curs
    in navigateMultiples r ((it + 1), nextNodes, (drop 1 ss))

getCycleLength :: Steps -> Routing -> String -> Int
getCycleLength ss r start =
  let (steps, next, seen, path) = getPathUntilRepeat r (ss, start, HashSet.singleton start, [start])
  in length path

stepsP2 :: String -> Int
stepsP2 l =
  let ls = lines l
      steps = getSteps ls
      routing = getRouting ls
      startingNodes = filter endsInA $ HashMap.keys routing
      cycles = map (getCycleLength steps routing) startingNodes
  in foldl1' lcm cycles

main :: IO()
main = do
  fileContent <- readFile "inputs/08_p1.txt"
  let steps = stepsToReachEnd fileContent
      i = stepsP2 fileContent
  print steps
  print i
