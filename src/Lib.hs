module Lib
  ( someFunc
  )
where

import qualified Text.Parsec                   as Parsec
import           Text.Parsec.String
import           Text.Parsec.Char
import           Text.Parsec.Combinator

import           Data.Functor                   ( ($>) )
import           Control.Applicative            ( (<|>) )


data Suit = Spades | Clubs | Diamonds | Hearts deriving (Show)
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A deriving (Show)
data Card = Card Rank Suit deriving (Show)

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

parse :: Parser a -> String -> Either Parsec.ParseError a
parse p = Parsec.parse (p <* eof) ""

someFunc :: IO ()
someFunc = print $ parse card "9S"
