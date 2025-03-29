module LibPropertyTest (main, prop_sameHandIsDraw, prop_strToHandInverse, prop_judgeJankenSymmetric) where

import Test.QuickCheck
import Lib

newtype TestHand = TestHand { unTestHand :: Hand }
  deriving (Show)

instance Arbitrary TestHand where
  arbitrary = TestHand <$> elements [Rock, Scissors, Paper]

-- 同じ手を出したら必ず引き分けになる
prop_sameHandIsDraw :: TestHand -> Bool
prop_sameHandIsDraw (TestHand h) = judgeJanken h h == Draw

-- strToHandとhandToStrは互いに逆関数
prop_strToHandInverse :: TestHand -> Bool
prop_strToHandInverse (TestHand h) = strToHand (handToStr h) == Just h

-- じゃんけんの判定は対称的
prop_judgeJankenSymmetric :: TestHand -> TestHand -> Bool
prop_judgeJankenSymmetric (TestHand h1) (TestHand h2) =
  case judgeJanken h1 h2 of
    Draw -> judgeJanken h2 h1 == Draw
    Win -> judgeJanken h2 h1 == Lose
    Lose -> judgeJanken h2 h1 == Win

main :: IO ()
main = do
  putStrLn "\nRunning QuickCheck tests..."
  quickCheck prop_sameHandIsDraw
  quickCheck prop_strToHandInverse
  quickCheck prop_judgeJankenSymmetric 