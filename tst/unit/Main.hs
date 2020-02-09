module Main where

import           Board
import           Test.HUnit
import           TestBoard


main :: IO ()
main = do
  runTestTT getters
  runTestTT setters
  pure ()
