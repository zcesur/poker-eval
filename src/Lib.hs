module Lib
  ( someFunc
  )
where


data Suit = Spades | Clubs | Diamonds | Hearts deriving (Show)
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A deriving (Show)
data Card = Card Rank Suit deriving (Show)

toCard :: String -> Maybe Card
toCard [r, s] = Card <$> toRank r <*> toSuit s
toCard _      = Nothing

toRank :: Char -> Maybe Rank
toRank '2' = Just R2
toRank '3' = Just R3
toRank '4' = Just R4
toRank '5' = Just R5
toRank '6' = Just R6
toRank '7' = Just R7
toRank '8' = Just R8
toRank '9' = Just R9
toRank 'T' = Just R10
toRank 'J' = Just J
toRank 'Q' = Just Q
toRank 'K' = Just K
toRank 'A' = Just A
toRank _   = Nothing

toSuit :: Char -> Maybe Suit
toSuit 'S' = Just Spades
toSuit 'C' = Just Clubs
toSuit 'D' = Just Diamonds
toSuit 'H' = Just Hearts
toSuit _   = Nothing

someFunc :: IO ()
someFunc = print $ toCard "9S"
