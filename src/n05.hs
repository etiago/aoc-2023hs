import qualified Data.Text as T
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Data.List (find)
import Data.List.Split (chunksOf)
import Data.List (foldl')

data MapperGroup = MapperGroup { mappers :: [Mapper] }
data Mapper = Mapper { dstStart :: Int, srcStart :: Int, count :: Int }
data MappingInput = MappingInput { seeds :: [Int], mapperGroups :: [MapperGroup] }

extractIntsFromValStr :: String -> [Int]
extractIntsFromValStr = map read . words

getMapperGroupFromChunk :: T.Text -> MapperGroup
getMapperGroupFromChunk t = MapperGroup
                            $ map (\v -> Mapper (v !! 0) (v !! 1) (v !! 2))
                            $ map extractIntsFromValStr . map T.unpack $ tail $ T.lines t

getMappers :: [T.Text] -> [MapperGroup]
getMappers ts =
  let unlined = T.unlines ts
      spl = T.splitOn (T.pack "\n\n") unlined
      mappersChunks = tail spl
  in map getMapperGroupFromChunk mappersChunks

mapMaybeMgToInt :: Int -> Maybe Mapper -> Int
mapMaybeMgToInt s m =
  case m of
    Just mapper -> (s - (srcStart mapper)) + (dstStart mapper)
    Nothing -> s

mapSingleSeedToNextInt :: Int -> MapperGroup -> Int
mapSingleSeedToNextInt s mg =
  let matchingMapper = find (\m -> s >= (srcStart m) && s < (count m) + (srcStart m)) (mappers mg)
  in mapMaybeMgToInt s matchingMapper

mapSeeds :: MappingInput -> [Int]
mapSeeds mi =
  map (\s -> foldl' mapSingleSeedToNextInt s (mapperGroups mi)) (seeds mi)

getSeeds :: [T.Text] -> [MapperGroup] -> MappingInput
getSeeds ts =
  let seedStr = T.splitOn (T.pack ": ") (head ts) !! 1
  in MappingInput $ map read (words $ T.unpack seedStr)

getSeedsAsRanges :: [T.Text] -> [MapperGroup] -> MappingInput
getSeedsAsRanges ts =
  let seedStr = T.splitOn (T.pack ": ") (head ts) !! 1
      chunkedInts :: [[Int]]
      chunkedInts = chunksOf 2 $ map read $ words $ T.unpack seedStr
      range = concat $ map (\c -> [(c !! 0)..((c!!0) + (c !! 1))]) chunkedInts
  in MappingInput range --map read (words $ T.unpack seedStr)

getMappingInput :: [T.Text] -> MappingInput
getMappingInput ts = getSeeds ts $ getMappers ts

getMappingInputWithSeedsAsRanges :: [T.Text] -> MappingInput
getMappingInputWithSeedsAsRanges ts = getSeedsAsRanges ts $ getMappers ts

parseInput :: String -> [T.Text]
parseInput = T.lines . T.pack

getLowestLocation :: String -> Int
getLowestLocation = minimum . mapSeeds . getMappingInput . parseInput

getLowestLocationForSeedRanges :: String -> Int
getLowestLocationForSeedRanges = minimum . mapSeeds . getMappingInputWithSeedsAsRanges . parseInput

main :: IO()
main = do
  fileContent <- readFile "inputs/05_p1.txt"
  let lowestLocation = getLowestLocation fileContent
      lowestLocationP2 = getLowestLocationForSeedRanges fileContent
  print lowestLocation
  print lowestLocationP2
