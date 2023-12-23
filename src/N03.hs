module N03 (main) where

main :: IO()
main = do
  fileContent <- readFile "inputs/03_example.txt"
  print fileContent
