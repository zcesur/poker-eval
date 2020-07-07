{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Lib
  ( someFunc
  )
where

import qualified Text.Parsec                   as Parsec
import           Text.Parsec.String
import           Text.Parsec.Char
import           Text.Parsec.Combinator

import           GHC.TypeLits
import           Data.Finite
import qualified Data.Vector.Sized             as V
import           Data.Vector.Sized              ( (//) )

import           Data.Functor                   ( ($>) )
import           Control.Applicative            ( (<|>) )
import           Control.Monad.State

data Suit = Spades | Clubs | Diamonds | Hearts deriving (Show, Eq, Ord)
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A deriving (Show, Eq, Ord, Enum)
data Card = Card Rank Suit deriving (Show, Eq, Ord)

type HandIndex = Finite 5
type Hand = V.Vector 5 Card
type Ranks n = V.Vector n Rank
type Suits n = V.Vector n Suit

data HandEval = StraightFlush Rank
              | Quads Rank Rank
              | FullHouse Rank Rank
              | Flush (Ranks 5)
              | Straight Rank
              | Trips Rank (Ranks 2)
              | TwoPair Rank Rank Rank
              | OnePair Rank (Ranks 3)
              | HighCard (Ranks 5)
              deriving (Show)

handIndices :: V.Vector 5 HandIndex
handIndices = V.generate id

suit :: Parser Suit
suit =
  (char 'S' $> Spades)
    <|> (char 'C' $> Clubs)
    <|> (char 'D' $> Diamonds)
    <|> (char 'H' $> Hearts)

rank :: Parser Rank
rank =
  (char '2' $> R2)
    <|> (char '3' $> R3)
    <|> (char '4' $> R4)
    <|> (char '5' $> R5)
    <|> (char '6' $> R6)
    <|> (char '7' $> R7)
    <|> (char '8' $> R8)
    <|> (char '9' $> R9)
    <|> (char 'T' $> R10)
    <|> (char 'J' $> J)
    <|> (char 'Q' $> Q)
    <|> (char 'K' $> K)
    <|> (char 'A' $> A)

card :: Parser Card
card = Card <$> rank <*> suit

hand :: Parser Hand
hand = V.cons <$> card <*> V.replicateM (space >> card)

hands :: Parser (Hand, Hand)
hands = (,) <$> hand <*> (space >> hand)

parse :: Parser a -> String -> Either Parsec.ParseError a
parse p = Parsec.parse (p <* eof) ""

sort :: Hand -> Hand
sort cs = snd $ execState (mapM_ move handIndices) (cs, cs)
 where
  pop :: State (Hand, Hand) Card
  pop = state $ \(cs, cs') ->
    (V.maximum cs, (cs // [(V.maxIndex cs, V.minimum cs)], cs'))

  push :: HandIndex -> Card -> State (Hand, Hand) ()
  push i c = state $ \(cs, cs') -> ((), (cs, cs' // [(i, c)]))

  move :: HandIndex -> State (Hand, Hand) ()
  move i = pop >>= push i

toRank :: Card -> Rank
toRank (Card r _) = r

toSuit :: Card -> Suit
toSuit (Card _ s) = s

same :: (Eq a) => V.Vector (1 + n) a -> Bool
same xs = all (== V.head xs) xs

isFlush :: Suits 5 -> Bool
isFlush = same

-- TODO: Implement five-high straight
isStraight :: Ranks 5 -> Bool
isStraight rs = same sums
  where sums = V.imap (\i r -> fromInteger (getFinite i) + fromEnum r) rs

eval :: Hand -> HandEval
eval cs | isStraight rs && isFlush ss = StraightFlush high
        | isFlush ss                  = Flush rs
        | isStraight rs               = Straight high
        | otherwise                   = HighCard rs
 where
  high = V.head rs
  rs   = V.map toRank cs'
  ss   = V.map toSuit cs'
  cs'  = sort cs

someFunc :: IO ()
someFunc = do
  print $ eval <$> parse hand "KC TC QC 9C JC"
  print $ eval <$> parse hand "8S TS KS 9S 4S"
  print $ eval <$> parse hand "8S TC QH 9D JC"
  print $ eval <$> parse hand "8C TS KC 9H 4S"
