module N04 (main) where

import qualified Data.Text as T
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.List (foldl')

data Card = Card { index :: Int, numbers :: [Int], winningNumbers :: [Int], count :: Int } deriving (Show)
data CardsState = CardsState { totalCardCount :: Int,
                               allCards :: [Card],
                               cardCounts :: (HashMap.HashMap Int Int) } deriving (Show)

parseSingleCard :: T.Text -> Card
parseSingleCard t =
  let cardNumberHalves = T.splitOn (T.pack ": ") t
      cardIndex = read (T.unpack (((T.words (cardNumberHalves !! 0)) !! 1)))
      valuesHalf = cardNumberHalves !! 1
      cardHalves = T.splitOn (T.pack " | ") valuesHalf
      cardHalvesTokenized = map T.words cardHalves
      cardHalvesTokenizedStr = map (map T.unpack) cardHalvesTokenized
      cardHalvesInts = map (map read) cardHalvesTokenizedStr
  in Card (cardIndex - 1) (head cardHalvesInts) (cardHalvesInts !! 1) 1

parseCards :: String -> [Card]
parseCards inputs =
  let inputText = T.pack inputs
      inputLines = T.lines inputText
  in map parseSingleCard inputLines

calculateCardPoints :: Card -> Int
calculateCardPoints card =
  let cardNumbersSet = HashSet.fromList (numbers card)
      winningNumbersSet = HashSet.fromList (winningNumbers card)
      intersection = (HashSet.intersection cardNumbersSet winningNumbersSet)
      intersectionSize = HashSet.size intersection
  in if intersectionSize == 0
     then 0
     else 2 ^ ((HashSet.size intersection) - 1)

getNewCardIndices :: [Card] -> Card -> [Int]
getNewCardIndices _ c =
  let cardNumbersSet = HashSet.fromList (numbers c)
      winningNumbersSet = HashSet.fromList (winningNumbers c)
      winningNumbersCount = (HashSet.size $ HashSet.intersection winningNumbersSet cardNumbersSet)
  in [((index c) + 1)..((index c) + winningNumbersCount)]

increaseCountOfCardInState :: Int -> CardsState -> Int -> CardsState
increaseCountOfCardInState increaseAmount state idx =
  let oldCardCounts = (cardCounts state)
      oldCount = oldCardCounts HashMap.! idx
      newCardCounts = HashMap.insert idx (oldCount + increaseAmount) oldCardCounts
  in state { cardCounts = newCardCounts }

getNewSetOfCards :: CardsState -> Card -> CardsState
getNewSetOfCards initialCardsState card =
  let cardIdx = (index card)
      actualCard = (allCards initialCardsState) !! cardIdx
      cardCount = (cardCounts initialCardsState) HashMap.! cardIdx
      newCounts = HashMap.insert cardIdx 0 (cardCounts initialCardsState)
      stateWithDecreasedCount = initialCardsState { cardCounts = newCounts }
      newCardIndicesToCheck = getNewCardIndices (allCards initialCardsState) actualCard
      newState = foldl' (increaseCountOfCardInState cardCount) stateWithDecreasedCount newCardIndicesToCheck
  in initialCardsState { totalCardCount = (((length newCardIndicesToCheck) * cardCount) + (totalCardCount initialCardsState)),
                         allCards = (allCards newState),
                         cardCounts = (cardCounts newState) }

allCardsAreZero :: CardsState -> Bool
allCardsAreZero currentState =
  all
    (== 0)
    (HashMap.elems (cardCounts currentState))

transformState :: CardsState -> CardsState
transformState currentState =
  (foldl' getNewSetOfCards currentState (allCards currentState))

getPartTwoNewCardsAltAlt :: CardsState -> CardsState
getPartTwoNewCardsAltAlt currentState =
  until allCardsAreZero transformState currentState

main :: IO()
main = do
  fileContent <- readFile "inputs/04_p1.txt"
  let cards = parseCards fileContent
      cardPoints = map calculateCardPoints cards
      cardCounts = foldl' (\acc c -> HashMap.insert (index c) 1 acc) HashMap.empty cards
  print (foldl' (+) 0 cardPoints)
  print (totalCardCount (getPartTwoNewCardsAltAlt (CardsState (length cards) cards cardCounts)))
