module Main where

import           Text.Parsec                    ( parse )
import           PokerEval                      ( hands )

main :: IO ()
main = go 0
 where
  go :: Integer -> IO ()
  go n = do
    putStrLn $ "P1 score: " ++ show n
    line <- getLine
    if (uncurry compare <$> parse hands "" line) == Right GT
      then go (n + 1)
      else go n
