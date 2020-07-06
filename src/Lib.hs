{-# LANGUAGE DataKinds                  #-}

module Lib
  ( someFunc
  )
where

import qualified Text.Parsec                   as Parsec
import           Text.Parsec.String
import           Text.Parsec.Char
import           Text.Parsec.Combinator

import           Data.Finite
import qualified Data.Vector.Sized             as V
import           Data.Vector.Sized              ( (//) )

import           Data.Functor                   ( ($>) )
import           Control.Applicative            ( (<|>) )
import           Control.Monad.State

data Suit = Spades | Clubs | Diamonds | Hearts deriving (Show, Eq, Ord)
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A deriving (Show, Eq, Ord)
data Card = Card Rank Suit deriving (Show, Eq, Ord)

type HandIndex = Finite 5
type Hand = V.Vector 5 Card

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

someFunc :: IO ()
someFunc = print $ sort . fst <$> parse hands "8C TS KC 9H 4S 7D 2S 5D 3S AC"
