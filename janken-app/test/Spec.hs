import Test.HUnit (runTestTT)
import LibUnitTest (unitTests)
import qualified LibPropertyTest

main :: IO ()
main = do
  putStrLn "Running HUnit tests..."
  _ <- runTestTT unitTests
  LibPropertyTest.main
