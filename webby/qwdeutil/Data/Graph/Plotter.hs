module Data.Graph.Plotter
  where

import Data.List (genericLength, sort)
import Text.Printf (printf)

data Plot = Plot {
  yTicks :: [Int]
  , xTicks :: [Int]
  , xAxis :: Axis
  , yAxis :: Axis
  } deriving (Eq, Show)

data Axis = Axis {
  x1 :: Int
  , x2 :: Int
  , y1 :: Int
  , y2 :: Int
  , labels :: [String]
  , labelPoints :: [Int]
  } deriving (Eq, Show)

mapToXticks :: Int -> Int -> [Int]
mapToXticks len numPx
  | len < 1 = []
  | len <= halfsize = take len $ iterate (+ inc) axisWidth
  | len < numPx = map floor $ take len $ iterate (+ incD) (fromIntegral axisWidth)
  | otherwise = [1..numPx]
  where
    usablePx = numPx - axisWidth
    inc = floor $ (fromIntegral usablePx :: Double) / ((fromIntegral . pred) len)
    incD = (fromIntegral usablePx :: Double) / ((fromIntegral . pred) len)
    halfsize = (ceiling $ ((fromIntegral numPx :: Double) / 2))

axisWidth :: Int
axisWidth = 90
axisHeight :: Int
axisHeight = fontHeight
fontHeight :: Int
fontHeight = 13

{--
   numLabels = how often to print a label
   pxHeight = how many, typically pixels, to disperse the dataset on
--}
--TODO: labelpoints are probably wrong.
getPlot :: Int -> Int -> Int -> [Double] -> [String] -> Plot
getPlot numLabels pxWidth pxHeight yli xli
  | or [null yli, null xli] = let axis = Axis 0 0 0 0 [] [] in Plot [] [] axis axis
  | otherwise = Plot { 
      yTicks = yTickies
      , xTicks = mapToXticks (length yTickies) pxWidth
      , xAxis = Axis { x1 = axisWidth, x2 = axisWidth, y2 = 0, y1 = pxHeight - fontHeight, labelPoints = xAxisLabelPoints, labels = xLabels }
      , yAxis = Axis { x1 = axisWidth, x2 = pxWidth, y2 = pxHeight - fontHeight, y1 = pxHeight - fontHeight, labelPoints = yAxisLabelPoints, labels = yLabels }

    } where
      yLabels = map (printf "%.2f") $ actualLabels
      yTickies = map ((\x -> subtract x (sph - fontHeight)) . round . (* yValToPxRation) . (subtract (minimum ticks))) ticks
      xLabels = labelsFunc xli --each (((fromIntegral . length) xli :: Double) / fromIntegral numLabels) xli
      yValToPxRation = (fromIntegral (sph - sph)) / (maximum ticks - minimum ticks)
      xAxisLabelPoints = 
        let inc = floor $ (fromIntegral (pxWidth - axisWidth) :: Double) / ((fromIntegral . pred) numLabels) :: Int
        in take numLabels $ iterate (+ inc) axisWidth :: [Int]
      yAxisLabelPoints = 
        let inc = floor $ (fromIntegral (sph - fontHeight) :: Double) / ((fromIntegral . pred) numLabels) :: Int
        in take numLabels $ iterate (subtract inc) sph :: [Int]
      -- StartPoint
      sph = pxHeight - fontHeight
      ticks = map average groups
      groups = grouper yli (\l -> length l <= numGroups) [] []
      numGroups = ceiling $ ((fromIntegral . length) yli :: Double) / (fromIntegral pxHeight)
      labelsFunc [] = [] 
      labelsFunc (x:[]) = [x]
      labelsFunc (x:y:[]) = [x,y]
      labelsFunc li'
        | (length li' <= numLabels) = li'
        | otherwise = each numLabels li'
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
