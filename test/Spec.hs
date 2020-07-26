import           Test.Hspec
import           Text.Parsec                    ( parse )
import           Data.List                      ( foldl' )
import           PokerEval

main :: IO ()
main = hspec $ do
  describe "PokerEval" $ do
    it ("correctly determines how many hands Player 1 wins in " ++ fname) $ do
      f <- readFile fname
      (foldl' tally 0 . map ord . lines) f `shouldBe` 376
 where
  fname = "poker.txt"
  ord   = fmap (uncurry compare) . parse hands fname
  tally acc x = if x == Right GT then acc + 1 else acc
