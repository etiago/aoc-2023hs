import Data.Maybe (catMaybes)
import GHC.Generics (K1)

data Race = Race { duration :: Int, recordDistance :: Int }

getDistanceBetterThanRecord :: Int -> Race -> Maybe Int
getDistanceBetterThanRecord holdDuration r =
  let travelDuration = (duration r) - holdDuration
      distance = holdDuration * travelDuration
  in if distance > (recordDistance r) then
    Just holdDuration
     else
    Nothing

getAllDistancesBetterThanRecord :: Race -> [Int]
getAllDistancesBetterThanRecord r =
  catMaybes $ map (\c -> getDistanceBetterThanRecord c r) [1..((duration r) - 1)]

getCountOfDistancesBetterThanRecord :: Race -> Int
getCountOfDistancesBetterThanRecord =
  length . getAllDistancesBetterThanRecord

getPartOneResult :: [Race] -> Int
getPartOneResult rs =
  foldl (*) 1 (map getCountOfDistancesBetterThanRecord rs)

main :: IO()
main = do
  let exampleRaces = [Race 7 9, Race 15 40, Race 30 200]
      races = [Race 46 214, Race 80 1177, Race 78 1402, Race 66 1024]
      partTwoRace = [Race 46807866 214117714021024]
  print $ getPartOneResult races
  print $ getPartOneResult partTwoRace
