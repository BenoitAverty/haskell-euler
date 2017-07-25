import Data.List
import Data.Maybe
import Control.Monad

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Enum, Bounded, Show)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Enum, Bounded, Show)
data Card = Card Suit Rank deriving (Show)
data Hand = Hand [Card] deriving (Show)
data Game = Game { player1 :: Hand, player2 :: Hand } deriving (Show)

readCard :: String -> Card
readCard [r,s] = Card (parseSuit s) (parseRank r)
  where parseRank '2' = Two
        parseRank '3' = Three
        parseRank '4' = Four
        parseRank '5' = Five
        parseRank '6' = Six
        parseRank '7' = Seven
        parseRank '8' = Eight
        parseRank '9' = Nine
        parseRank 'T' = Ten
        parseRank 'J' = Jack
        parseRank 'Q' = Queen
        parseRank 'K' = King
        parseRank 'A' = Ace
        parseSuit 'S' = Spades
        parseSuit 'H' = Hearts
        parseSuit 'D' = Diamonds
        parseSuit 'C' = Clubs

parseGameLine :: String -> Game
parseGameLine = buildGame . splitAt 5 . map readCard . words
  where buildGame (h1,h2) = Game (Hand h1) (Hand h2)

answer = do
    f <- readFile "euler54_data.txt"
    let games = map parseGameLine $ lines f
    print $ games
