module Main where

import           Test.HUnit
import           TestBoard


main :: IO ()
main = do
  runTestTT getters
  runTestTT setters
  pure ()
