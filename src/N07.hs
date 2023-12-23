module N07 (main) where

import qualified Data.HashMap.Strict as HashMap
import Data.List (elemIndex, sortBy, foldl')
import Data.Maybe (fromJust, mapMaybe)

data CardSequence = CardSequence { cards :: [Char], handType :: HandType, bid :: Int } deriving (Show)
data HandType = FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard deriving (Eq, Show)

handsInRankOrder :: [HandType]
handsInRankOrder = [FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair, HighCard]
cardsInRankOrder :: [Char]
cardsInRankOrder = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']
cardsInNewRankOrder :: [Char]
cardsInNewRankOrder = ['A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J']

cardsToRank :: [Char] -> HashMap.HashMap Char Int
cardsToRank cardRanks = HashMap.fromList (zip (reverse cardRanks) [0..])

leftCardIsBigger :: [Char] -> Char -> Char -> Maybe Bool
leftCardIsBigger cardRanks left right =
  let sizeLeft = cardsToRank cardRanks HashMap.! left
      sizeRight = cardsToRank cardRanks HashMap.! right
  in if sizeLeft == sizeRight then
    Nothing
  else
    Just $ sizeLeft > sizeRight

leftCardSeqIsBigger :: [Char] -> CardSequence -> CardSequence -> Bool
leftCardSeqIsBigger cardRanks left right =
  let leftIdx = fromJust (elemIndex (handType left) handsInRankOrder)
      rightIdx = fromJust (elemIndex (handType right) handsInRankOrder)
      leftsAreBigger = mapMaybe (\(c1, c2) -> (leftCardIsBigger cardRanks c1 c2)) (zip (cards left) (cards right))
  in if leftIdx == rightIdx then
    if null leftsAreBigger then
      True -- how to deal with this
    else
      head leftsAreBigger
  else
    leftIdx < rightIdx

cardSequenceRank :: [Char] -> CardSequence -> CardSequence -> Ordering
cardSequenceRank cardRanks csLeft csRight =
  if leftCardSeqIsBigger cardRanks csLeft csRight then
    GT
  else
    LT

getHandTypeFromCards :: [Char] -> HandType
getHandTypeFromCards cs =
  let counts :: HashMap.HashMap Char Int
      counts = foldl (\acc c -> HashMap.insertWith (+) c 1 acc) HashMap.empty cs
      els = HashMap.elems counts
      freqs :: HashMap.HashMap Int Int
      freqs = foldl (\acc c -> HashMap.insertWith (+) c 1 acc) HashMap.empty els
  in if 1 == HashMap.lookupDefault 0 5 freqs then
    FiveOfAKind
  else if 1 == HashMap.lookupDefault 0 4 freqs then
    FourOfAKind
  else if 1 == HashMap.lookupDefault 0 3 freqs && 1 == HashMap.lookupDefault 0 2 freqs then
    FullHouse
  else if 1 == HashMap.lookupDefault 0 3 freqs then
    ThreeOfAKind
  else if 2 == HashMap.lookupDefault 0 2 freqs then
    TwoPair
  else if 1 == HashMap.lookupDefault 0 2 freqs then
    OnePair
  else HighCard

getCountOfCardInCards :: Char -> [Char] -> Int
getCountOfCardInCards c =
  foldl' (\acc curC -> if curC == c then acc + 1 else acc) 0

upgradeHandTypeWithJokers :: [Char] -> HandType -> HandType
upgradeHandTypeWithJokers cs curHandType =
  if curHandType == FiveOfAKind then
    FiveOfAKind
  else
    let jokerCount = getCountOfCardInCards 'J' cs
    in if curHandType == FourOfAKind && jokerCount > 0 then
      FiveOfAKind
    else if curHandType == FullHouse && jokerCount > 0 then
      FiveOfAKind
    else if curHandType == ThreeOfAKind && (jokerCount == 1 || jokerCount == 3) then
      FourOfAKind
    else if curHandType == TwoPair && jokerCount == 2 then
      FourOfAKind
    else if curHandType == TwoPair && jokerCount == 1 then
      FullHouse
    else if curHandType == OnePair && (jokerCount == 2 || jokerCount == 1) then
      ThreeOfAKind
    else if curHandType == HighCard && jokerCount == 1 then
      OnePair
    else curHandType


parseSingleCardSequence :: String -> CardSequence
parseSingleCardSequence l =
  let hand = take 5 l
      bid = read $ drop 6 l
  in CardSequence hand (getHandTypeFromCards hand) bid

readAllCardSequences :: String -> [CardSequence]
readAllCardSequences s =
  let ls = lines s
  in map parseSingleCardSequence ls

getTotalWinnings :: [(Int, Int)] -> Int
getTotalWinnings = foldl (\sum rb -> sum + ((fst rb) * (snd rb))) 0

upgradeCardSequencesWithJokers :: CardSequence -> CardSequence
upgradeCardSequencesWithJokers CardSequence {cards = cards, handType = handType, bid = bid} =
  CardSequence cards (upgradeHandTypeWithJokers cards handType) bid

main :: IO()
main = do
  fileContent <- readFile "inputs/07_p1.txt"
  let cardSequences = readAllCardSequences fileContent
      csSortByRank = sortBy (cardSequenceRank cardsInRankOrder) cardSequences
      upgradedCardSequences = map upgradeCardSequencesWithJokers cardSequences
      upgradedSorted = sortBy (cardSequenceRank cardsInNewRankOrder) upgradedCardSequences

  print $ getTotalWinnings $ zip [1..] $ map bid csSortByRank
  print $ getTotalWinnings $ zip [1..] $ map bid upgradedSorted
