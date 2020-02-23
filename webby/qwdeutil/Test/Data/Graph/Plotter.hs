{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Data.Graph.Plotter
  (tests)
  where

import Data.Graph.Plotter

import           Test.HUnit (Test (..), assertBool, assertEqual)

test_basic :: Test
test_basic = TestCase $
  let inData = [1..10] :: [Double]
      plot = getPlot 10 10 10 inData ["a", "b", "c"]
    in
      assertEqual "min" 1.0 (yMin plot) >>
      assertEqual "max" 10.0 (yMax plot) >>
      assertEqual "labels" [1..10] (yLabels plot) >>
      assertEqual (show $ yTicks plot) inData (yTicks plot)

test_shorten :: Test
test_shorten = TestCase $
  let inData = [1,2,3] :: [Double]
      plot = getPlot 2 2 10 inData ["a", "b", "c"]
    in
      assertEqual "min" 1.5 (yMin plot) >>
      assertEqual "max" 3.0 (yMax plot) >>
      assertEqual "labels" [1.5, 3.0] (yLabels plot) >>
      assertEqual (show $ yTicks plot)  [1.5, 3.0] (yTicks plot)

tests :: Test
tests = TestList [ TestLabel "basic" test_basic
                 , TestLabel "group" test_shorten

                 ]
