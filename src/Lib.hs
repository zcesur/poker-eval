{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Lib
  ( hands
  , cmpEval
  )
where

import           Text.Parsec.String
import           Text.Parsec.Char

import           GHC.TypeLits
import           Data.Finite
import qualified Data.List                     as L
import qualified Data.List.NonEmpty            as NE
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

straight :: Ranks 5 -> Maybe Rank
straight rs | same sums                             = Just (V.head rs)
            | V.toList rs == A : reverse [R2 .. R5] = Just R5
            | otherwise                             = Nothing
  where sums = V.imap (\i r -> fromInteger (getFinite i) + fromEnum r) rs

groups :: Hand -> [(Int, Rank)]
groups =
  L.sortBy (flip compare)
    . map (\rs -> (length rs, NE.head rs))
    . NE.group
    . V.toList
    . V.map toRank

evalGroups :: Hand -> Maybe HandEval
evalGroups xs = case groups xs of
  [(4, x), (1, y)]         -> Just (Quads x y)
  [(3, x), (2, y)]         -> Just (FullHouse x y)
  [(3, x), (1, y), (1, z)] -> Just (Trips x (V.fromTuple (y, z)))
  [(2, x), (2, y), (1, z)] -> Just (TwoPair x y z)
  [(2, x), (1, y), (1, z), (1, t)] -> Just (OnePair x (V.fromTuple (y, z, t)))
  _                        -> Nothing

eval :: Hand -> HandEval
eval cs | Just h <- str, same ss       = StraightFlush h
        | Just (Quads q k) <- gs       = Quads q k
        | Just (FullHouse t p) <- gs   = FullHouse t p
        | same ss                      = Flush rs
        | Just h <- str                = Straight h
        | Just (Trips t ks) <- gs      = Trips t ks
        | Just (TwoPair p1 p2 k) <- gs = TwoPair p1 p2 k
        | Just (OnePair p ks) <- gs    = OnePair p ks
        | otherwise                    = HighCard rs
 where
  rs  = V.map toRank cs'
  ss  = V.map toSuit cs'
  gs  = evalGroups cs'
  str = straight rs
  cs' = sort cs

cmpEval :: Hand -> Hand -> Ordering
cmpEval h1 h2 = compare (eval h1) (eval h2)
