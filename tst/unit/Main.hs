module Main where

import           Game
import           Logic
import           Test.HUnit
import           TestBoard


main :: IO ()
main = do
  runTestTT getters
  runTestTT setters
  pure ()
