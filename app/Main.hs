module Main where

import           Text.Parsec                    ( parse )
import           Text.Parsec.String             ( Parser )
import           Text.Parsec.Combinator         ( eof )
import           PokerEval                      ( hands )

main :: IO ()
main = go 0
 where
  go n = do
    putStrLn $ "P1 score: " ++ show n
    line <- getLine
    if (uncurry compare <$> parse hands "" line) == Right GT
      then go (n + 1)
      else go n
