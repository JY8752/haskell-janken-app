module Lib
  ( playJanken,
    Hand (..),
    Result (..),
    handToStr,
    strToHand,
    judgeJanken,
  )
where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO as IO
import System.Random (randomRIO)

-- じゃんけんの手を表す型
data Hand = Rock | Scissors | Paper
  deriving (Eq, Show, Read)

-- 勝敗の結果を表す型
data Result = Draw | Win | Lose
  deriving (Eq, Show)

-- 日本語表記への変換
handToStr :: Hand -> String
handToStr Rock = "グー"
handToStr Scissors = "チョキ"
handToStr Paper = "パー"

-- 結果の日本語表記への変換
resultToStr :: Result -> String
resultToStr Draw = "あいこです"
resultToStr Win = "あなたの勝ちです"
resultToStr Lose = "あなたの負けです"

-- 日本語入力からHandへの変換
strToHand :: String -> Maybe Hand
strToHand "グー" = Just Rock
strToHand "チョキ" = Just Scissors
strToHand "パー" = Just Paper
strToHand _ = Nothing

-- ランダムな手を生成
randomHand :: IO Hand
randomHand = do
  n <- randomRIO (0, 2) :: IO Int
  return $ case n of
    0 -> Rock
    1 -> Scissors
    _ -> Paper

-- 勝敗判定
judgeJanken :: Hand -> Hand -> Result
judgeJanken player computer
  | player == computer = Draw
  | (player == Rock && computer == Scissors)
      || (player == Scissors && computer == Paper)
      || (player == Paper && computer == Rock) =
      Win
  | otherwise = Lose

-- じゃんけんゲームを実行
playJanken :: IO ()
playJanken = do
  IO.hSetEncoding IO.stdin IO.utf8
  IO.hSetEncoding IO.stdout IO.utf8

  putStrLn "じゃんけんをしましょう！"
  putStrLn "「グー」、「チョキ」、「パー」のいずれかを入力してください："

  input <- TIO.getLine
  case strToHand (T.unpack input) of
    Nothing -> do
      putStrLn "入力が正しくありません。「グー」、「チョキ」、「パー」のいずれかを入力してください。"
      playJanken
    Just playerHand -> do
      computerHand <- randomHand

      putStrLn $ "あなた: " ++ handToStr playerHand
      putStrLn $ "コンピュータ: " ++ handToStr computerHand

      let result = judgeJanken playerHand computerHand
      putStrLn $ resultToStr result

      when (result == Draw) $ do
        putStrLn "もう一度じゃんけんをしましょう！"
        playJanken
