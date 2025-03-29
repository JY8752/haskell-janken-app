module LibUnitTest (unitTests) where

import Test.HUnit
import Lib

unitTests :: Test
unitTests = TestList
  [ "handToStr tests" ~: TestList
    [ "グー" ~: "グー" ~=? handToStr Rock
    , "チョキ" ~: "チョキ" ~=? handToStr Scissors
    , "パー" ~: "パー" ~=? handToStr Paper
    ]
  , "strToHand tests" ~: TestList
    [ "グー" ~: Just Rock ~=? strToHand "グー"
    , "チョキ" ~: Just Scissors ~=? strToHand "チョキ"
    , "パー" ~: Just Paper ~=? strToHand "パー"
    , "不正な入力" ~: Nothing ~=? strToHand "不正な入力"
    ]
  , "judgeJanken tests" ~: TestList
    [ "同じ手はあいこ" ~: Draw ~=? judgeJanken Rock Rock
    , "グーはチョキに勝つ" ~: Win ~=? judgeJanken Rock Scissors
    , "チョキはパーに勝つ" ~: Win ~=? judgeJanken Scissors Paper
    , "パーはグーに勝つ" ~: Win ~=? judgeJanken Paper Rock
    , "グーはパーに負ける" ~: Lose ~=? judgeJanken Rock Paper
    , "チョキはグーに負ける" ~: Lose ~=? judgeJanken Scissors Rock
    , "パーはチョキに負ける" ~: Lose ~=? judgeJanken Paper Scissors
    ]
  ]