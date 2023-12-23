module Advent2023 where

import qualified N01
import qualified N02
import qualified N03
import qualified N04
import qualified N05
import qualified N06
import qualified N07
import qualified N08
import qualified N08_Broken

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
  , N08_Broken.main
  ]

main :: IO ()
main = do
  N08_Broken.main
