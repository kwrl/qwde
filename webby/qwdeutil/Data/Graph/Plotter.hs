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
  , xAxis :: Axis
  , yAxis :: Axis
  } deriving (Eq, Show)

data Axis = Axis {
  x1 :: Int
  , x2 :: Int
  , y1 :: Int
  , y2 :: Int
  , labelPoints :: [Int]
  } deriving (Eq, Show)

{--
   numLabels = how often to print a label
   pxHeight = how many, typically pixels, to disperse the dataset on
--}
--TODO: labelpoints are probably wrong.
getPlot :: Int -> Int -> Int -> [Double] -> [String] -> Plot
getPlot numLabels pxWidth pxHeight yli xli = Plot { 
  yMax = maximum ticks
  , yMin = minimum ticks
  , yTicks = map ((subtract (minimum ticks)) . (/ (fromIntegral pxHeight))) ticks
  , yLabels = actualLabels
  , xTicks = xli
  , xLabels = labels xli --each (((fromIntegral . length) xli :: Double) / fromIntegral numLabels) xli
  , xAxis = Axis { x1 = 90, x2 = 90, y1 = 5, y2 = pxHeight -5, labelPoints = xAxisLabelPoints }
  , yAxis = Axis { x1 = 90, x2 = pxWidth - 90, y1 = pxHeight - 5, y2 = pxHeight - 5, labelPoints = yAxisLabelPoints }

} where
  
  xAxisLabelPoints = 
    let inc = ceiling $ (fromIntegral (pxWidth - 90) :: Double) / (fromIntegral numLabels) :: Int 
     in take numLabels $ iterate (+ inc) 90 :: [Int]
  yAxisLabelPoints = 
    let inc = ceiling $ (fromIntegral (pxHeight - 5) :: Double) / (fromIntegral numLabels) :: Int 
     in take numLabels $ iterate (+ inc) 5 :: [Int]
  ticks = map average groups
  groups = grouper yli (\l -> length l <= numGroups) [] []
  numGroups = ceiling $ ((fromIntegral . length) yli :: Double) / (fromIntegral pxHeight)
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
    | otherwise = let inc = (interval) / ((fromIntegral . pred) numLabels) in take numLabels $ iterate (+ inc) (minimum ticks)

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
