module Data.Graph.Plotter
  where

import Data.List (genericLength, sort)

data Plot = Plot {
  yMax :: Double
  , yMin :: Double
  , yTicks :: [Double]
  , yLabels :: [Double]
  , xLabels :: [String]
  , xTicks :: [String]
          } deriving (Show)

{--
   numLabels = how often to print a label
   numPoints = how many, typically pixels, to disperse the dataset on
--}
getPlot :: Int -> Int -> [Double] -> [String] -> Plot
getPlot numLabels numPoints yli xli = Plot { 
  yMax = maximum ticks
  , yMin = minimum ticks
  , yTicks = ticks
  , yLabels = actualLabels
  , xTicks = xli
  , xLabels = labels xli --each (((fromIntegral . length) xli :: Double) / fromIntegral numLabels) xli
} where
  ticks = map average groups
  groups = grouper yli (\l -> length l <= numGroups) [] []
  numGroups = ceiling $ ((fromIntegral . length) yli :: Double) / (fromIntegral numPoints)
  labels [] = [] 
  labels (x:[]) = [x]
  labels (x:y:[]) = [x,y]
  labels li'
    | (length li' <= numLabels) = sort li'
    | otherwise = each numLabels (sort li')
  interval = maximum ticks - minimum ticks
  actualLabels 
    | numLabels <= 0 = []
    | numLabels == 1 = [head ticks]
    | numLabels == 2 = head ticks : [last ticks]
    | otherwise = let inc = (interval) / ((fromIntegral . pred) numLabels) in take numLabels $ iterate (\x -> x + inc) (minimum ticks)

grouper :: [a] -> ([a] -> Bool) -> [a] -> [[a]] -> [[a]]
grouper (x:xs) func a r = let cand = a ++ [x] in if (func cand) then grouper xs (func) cand r else grouper (xs) (func) [x] (r ++ [a]) 
grouper [] _ a r = r ++ [a]

average :: Fractional a => [a] -> a
average xs = sum xs / genericLength xs

each :: Int -> [a] -> [a]
each n li
  | n <= 0 = li
  | n >= length li = head li : [last li]
  | n `rem` length li == 0 = map head . takeWhile (not . null) . iterate (drop n) $ li
  | otherwise = (map head . takeWhile (fAnd [(not . null), (\x -> (length x) > n)]) . iterate (drop n) $ li) ++ [last li]

fAnd :: Foldable t => t (a -> Bool) -> a -> Bool
fAnd fs x = all ($x) fs
