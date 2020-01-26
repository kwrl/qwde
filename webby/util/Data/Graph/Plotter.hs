module Data.Graph.Plotter
  where

import Data.List (inits, tails, genericLength, sort)

data Plot = Plot {
  yMax :: Double
  , yMin :: Double
  , yTicks :: [Double]
  , yLabels :: [Double]
          } deriving (Show)

{--
   numIntervals = how often to print a label
   numPoints = how many, typically pixels, to disperse the dataset on
--}
getPlot :: Int -> Int -> [Double] -> Plot
getPlot numIntervals numPoints li = Plot { 
  yMax = maximum ticks
  , yMin = minimum ticks
  , yTicks = ticks
  , yLabels = labels
} where
  ticks = map average groups
  groups = groupWhile (\l -> length l <= numGroups) li
  numGroups = ceiling $ ((fromIntegral . length) li :: Double) / (fromIntegral numPoints)
  numLabels = floor $ ((fromIntegral . length) li :: Double) / (fromIntegral numIntervals)
  labels = each numLabels (sort ticks)

groupWhile _    [] = []
groupWhile pred xs = let (a,b) = splitAtFunc pred xs in a : groupWhile pred b

splitAtFunc f xs = last $ filter (f . fst) (zip (inits xs) (tails xs))

average :: Fractional a => [a] -> a
average xs = sum xs / genericLength xs

each :: Int -> [a] -> [a]
each n = map head . takeWhile (not . null) . iterate (drop n)
