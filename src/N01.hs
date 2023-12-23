module N01 (main) where

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

digitsList :: [(String, String)]
digitsList = [("one", "one1one"),
               ("two", "two2two"),
               ("three", "three3three"),
               ("four", "four4four"),
               ("five", "five5five"),
               ("six", "six6six"),
               ("seven", "seven7seven"),
               ("eight", "eight8eight"),
               ("nine", "nine9nine")]

findAndReplaceAllDigits :: String -> String
findAndReplaceAllDigits str =
  let initialText = T.pack str
      replacedText = foldl (\acc (word, digit) -> T.replace (T.pack word) (T.pack digit) acc) initialText digitsList
  in T.unpack replacedText
  --let initialText = T.pack str
  --    replacedText = foldl (\acc (word, digit) -> findAndReplace (T.pack word) (T.pack digit) acc) initialText digitsList
  --in T.unpack replacedText

extractCalibration :: String -> Maybe Int
extractCalibration s =
  let digits = filter isDigit s
  in if not (null digits)
     then Just (read ([head digits] ++ [last digits]) :: Int)
     else Nothing

part1 :: [String] -> Int
part1 = sum . mapMaybe id . map extractCalibration

main ::  IO()
main = do
  fileContent <- readFile "inputs/01_p1.txt"
  print (part1 $ lines fileContent)
  let fileContentReplaced = findAndReplaceAllDigits fileContent
  print (part1 $ lines fileContentReplaced)
