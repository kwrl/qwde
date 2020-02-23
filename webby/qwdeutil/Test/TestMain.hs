module TestMain
  where

import Control.Monad
import qualified Test.Data.Graph.Plotter as Plotter
import Test.HUnit
import System.Exit (exitFailure)

getNumberOfFailures :: Counts -> Int
getNumberOfFailures (Counts _ _ _ f) = f

main :: IO ()
main = do
  -- runTest[T]ext(to)[T]erminal
  c <- runTestTT $ TestList [
    Plotter.tests
    ]

  when (getNumberOfFailures c > 0)
    exitFailure
