import qualified Data.Text as T

data RevealRound = RevealRound { red :: Int, green :: Int, blue :: Int } deriving (Show)
data Game = Game { reveals :: [RevealRound] } deriving (Show)

createRevealRound :: [(Int, String)] -> RevealRound
createRevealRound tuples = foldr assignColor (RevealRound 0 0 0) tuples
  where
    assignColor (value, "red") (RevealRound _ g b) = RevealRound value g b
    assignColor (value, "green") (RevealRound r _ b) = RevealRound r value b
    assignColor (value, "blue") (RevealRound r g _) = RevealRound r g value
    assignColor _ revealRound = revealRound

parseGames :: String -> [[RevealRound]]
parseGames games =
  let textInput = T.pack games
      linesOfText = T.lines textInput
      splitAndTakeRight input = T.splitOn (T.pack ": ") input !! 1
      splitGamePerReveal gameStr = T.splitOn (T.pack "; ") gameStr
      splitRevealPerColor revealStr = T.splitOn (T.pack ", ") revealStr
      parseColorStr colorStr = T.splitOn (T.pack " ") colorStr
      parsedColorStrToTuple parsedColor = (read (T.unpack (parsedColor !! 0)) :: Int, T.unpack (parsedColor !! 1))
      colorTuplesToRevealRound colorTuples = createRevealRound colorTuples
  in map (map colorTuplesToRevealRound .
          (map
           (map parsedColorStrToTuple .
            (map parseColorStr))))
     (map
      (map splitRevealPerColor)
      (map splitGamePerReveal
       (map splitAndTakeRight linesOfText)))

roundIsValid :: RevealRound -> Bool
roundIsValid round = (red round <= 12) && (green round <= 13) && (blue round <= 14)

gameIsValid :: [RevealRound] -> Bool
gameIsValid game = all roundIsValid game

newMinimumColors :: RevealRound -> RevealRound -> RevealRound
newMinimumColors acc round = RevealRound (max (red round) (red acc)) (max (green round) (green acc)) (max (blue round) (blue acc))

minimumColorsRequired :: [RevealRound] -> RevealRound
minimumColorsRequired rounds = foldl newMinimumColors (RevealRound 0 0 0) rounds

getCubesForRound :: RevealRound -> Int
getCubesForRound round = red round * green round * blue round

main :: IO()
main = do
  fileContent <- readFile "inputs/02_p2.txt"
  let games = parseGames fileContent
  --print games

  let validStateGames = (map gameIsValid games)
  let indexedValidStateGames = zip validStateGames [1..]
  let indexedOnlyValid = filter fst indexedValidStateGames
  let sum = foldl (\acc tuple -> acc + snd tuple) 0 indexedOnlyValid
  print sum

  let minimumColors = map minimumColorsRequired games
  let cubes = map getCubesForRound minimumColors
  let sumOfCubes = foldl (\acc c -> acc + c) 0 cubes

  print sumOfCubes
