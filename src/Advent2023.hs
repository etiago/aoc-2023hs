module Advent2023 (main) where

import qualified N01
import qualified N02
import qualified N03
import qualified N04
import qualified N05
import qualified N06
import qualified N07
import qualified N08
import qualified N09
import qualified N10
import qualified N11

mains :: [(Int,IO ())]
mains = zip [1..]
  [
    N01.main
  , N02.main
  , N03.main
  , N04.main
  , N05.main
  , N06.main
  , N07.main
  , N08.main
  , N09.main
  , N10.main
  , N11.main
  ]

main :: IO ()
main = do
  N11.main
  print (length mains)
