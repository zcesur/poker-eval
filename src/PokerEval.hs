{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}

module PokerEval
  ( hands
  )
where

import           Text.Parsec.String
import           Text.Parsec.Char

import           GHC.TypeLits
import           Data.Finite
import qualified Data.List                     as L
import qualified Data.List.NonEmpty            as NE
import qualified Data.Vector.Sized             as V

import           Data.Functor                   ( ($>) )
import           Control.Applicative            ( (<|>) )

data Card = Card Rank Suit deriving (Show, Eq, Ord)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum)
data Suit = Spades | Clubs | Diamonds | Hearts deriving (Show, Eq, Ord)

type Cards n = V.Vector n Card
type Ranks n = V.Vector n Rank
type Suits n = V.Vector n Suit

newtype Hand s = Hand (Cards 5)

data HandEval = HighCard (Ranks 5)
              | OnePair Rank (Ranks 3)
              | TwoPair Rank Rank Rank
              | Trips Rank (Ranks 2)
              | Straight Rank
              | Flush (Ranks 5)
              | FullHouse Rank Rank
              | Quads Rank Rank
              | StraightFlush Rank
              deriving (Show, Eq, Ord)

data Sorted
data Unsorted

class Sort a where
  sort :: Hand a -> Hand Sorted

instance Sort Sorted where
  sort = id

instance Sort Unsorted where
  sort (Hand cs) = Hand (vsort cs)

instance Sort a => Eq (Hand a) where
  h1 == h2 = eval h1 == eval h2

instance Sort a => Ord (Hand a) where
  compare h1 h2 = compare (eval h1) (eval h2)



-- MAIN


eval :: Sort a => Hand a -> HandEval
eval h | Just hi <- straight h', same ss = StraightFlush hi
       | Just (Quads q k) <- gs          = Quads q k
       | Just (FullHouse t p) <- gs      = FullHouse t p
       | same ss                         = Flush rs
       | Just hi <- straight h'          = Straight hi
       | Just (Trips t ks) <- gs         = Trips t ks
       | Just (TwoPair p1 p2 k) <- gs    = TwoPair p1 p2 k
       | Just (OnePair p ks) <- gs       = OnePair p ks
       | otherwise                       = HighCard rs
 where
  rs = ranks h'
  ss = suits h'
  gs = evalGroups h'
  h' = sort h

evalGroups :: Hand Sorted -> Maybe HandEval
evalGroups h = case groups h of
  [(4, x), (1, y)]         -> Just (Quads x y)
  [(3, x), (2, y)]         -> Just (FullHouse x y)
  [(3, x), (1, y), (1, z)] -> Just (Trips x (V.fromTuple (y, z)))
  [(2, x), (2, y), (1, z)] -> Just (TwoPair x y z)
  [(2, x), (1, y), (1, z), (1, t)] -> Just (OnePair x (V.fromTuple (y, z, t)))
  _                        -> Nothing

groups :: Hand Sorted -> [(Int, Rank)]
groups (Hand cs) =
  L.sortBy (flip compare)
    . map (\rs -> (length rs, NE.head rs))
    . NE.group
    . V.toList
    . V.map toRank
    $ cs

straight :: Hand Sorted -> Maybe Rank
straight h | same sums = Just (V.head rs)
           | V.toList rs == Ace : reverse [Two .. Five] = Just Five
           | otherwise = Nothing
 where
  rs   = ranks h
  sums = V.imap (\i r -> fromInteger (getFinite i) + fromEnum r) rs

toRank :: Card -> Rank
toRank (Card r _) = r

toSuit :: Card -> Suit
toSuit (Card _ s) = s

ranks :: Hand a -> Ranks 5
ranks (Hand cs) = V.map toRank cs

suits :: Hand a -> Suits 5
suits (Hand cs) = V.map toSuit cs



-- UTILS


data SortState = Sorted | Unsorted deriving (Eq, Ord)
type SVec n a = V.Vector n (SortState, a)

vsort :: (Ord a, KnownNat (n + 1)) => V.Vector (n + 1) a -> V.Vector (n + 1) a
vsort = fromSVec . go . toSVec
 where
  go :: (Ord a, KnownNat (n + 1)) => SVec (n + 1) a -> SVec (n + 1) a
  go cs = V.foldl' sortOne cs (V.generate id)

  sortOne :: Ord a => SVec (n + 1) a -> Finite (n + 1) -> SVec (n + 1) a
  sortOne cs n =
    cs V.// [(V.maxIndex cs, V.index cs n), (n, (Sorted, snd (V.maximum cs)))]

  toSVec :: V.Vector n a -> SVec n a
  toSVec = V.map ((,) Unsorted)

  fromSVec :: SVec n a -> V.Vector n a
  fromSVec = V.map snd

same :: (Eq a) => V.Vector (1 + n) a -> Bool
same xs = all (== V.head xs) xs



-- PARSERS


hand :: Parser (Hand Unsorted)
hand = Hand <$> (V.cons <$> card <*> V.replicateM (space >> card))

hands :: Parser (Hand Unsorted, Hand Unsorted)
hands = (,) <$> hand <*> (space >> hand)

card :: Parser Card
card = Card <$> rank <*> suit

suit :: Parser Suit
suit =
  (char 'S' $> Spades)
    <|> (char 'C' $> Clubs)
    <|> (char 'D' $> Diamonds)
    <|> (char 'H' $> Hearts)

rank :: Parser Rank
rank =
  (char '2' $> Two)
    <|> (char '3' $> Three)
    <|> (char '4' $> Four)
    <|> (char '5' $> Five)
    <|> (char '6' $> Six)
    <|> (char '7' $> Seven)
    <|> (char '8' $> Eight)
    <|> (char '9' $> Nine)
    <|> (char 'T' $> Ten)
    <|> (char 'J' $> Jack)
    <|> (char 'Q' $> Queen)
    <|> (char 'K' $> King)
    <|> (char 'A' $> Ace)
