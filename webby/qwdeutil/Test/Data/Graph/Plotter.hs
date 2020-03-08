{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Data.Graph.Plotter
  (tests)
  where

import Data.Graph.Plotter

import           Test.HUnit (Test (..), assertEqual)

test_empty :: Test
test_empty = TestCase $
  let inData = [] :: [Double]
      plot = getPlot 10 10 10 [] [inData]
    in
      assertEqual "labels" [] (labels $ xAxis plot) >>
      assertEqual "labels" [] (labels $ yAxis plot) >>
      assertEqual "yticks" (map round inData) ((yTicks . head . plotData) plot)

test_basic :: Test
test_basic = TestCase $
  let inData = [1..10] :: [Double]
      plot = getPlot 10 10 10 ["1.00", "2.00", "3.00", "3.00", "4.00", "6.00", "7.00", "8.00", "9.00", "10.00"] [inData]
    in
      assertEqual "labels" ["1.00","2.00","3.00","4.00","5.00","6.00","7.00","8.00","9.00","10.00"] (labels $ yAxis plot) >>
        -- TODO: not correct, but temporary ok.
      assertEqual "yTicks" ([-16, -14, -12, -11, -9, -7, -5, -4, -2, 0]) ((yTicks . head . plotData) plot)

test_shorten :: Test
test_shorten = TestCase $
  let inData = [[1,2,3]]
      plot = getPlot 2 2 10 ["a", "b", "c"] inData
    in
      assertEqual "labels" ["1.00", "3.00"] (labels $ yAxis plot)  >>
      assertEqual "yTicks" [-16, -8, 0] ((yTicks . head . plotData) plot)

test_labels :: Test
test_labels = TestCase $
  let inData = [[exp 1, pi]]
      plot = getPlot 2 2 2 ["a", "b"] inData
    in
      assertEqual "labels" ["2.72", "3.14"] (labels $ yAxis plot)  >>
      assertEqual "yTicks" [-24, 0] ((yTicks . head . plotData) plot)

tests :: Test
tests = TestList [ TestLabel "basic" test_basic
                 , TestLabel "group" test_shorten
                 , TestLabel "empty" test_empty
                 , TestLabel "labels" test_labels
                 ]
