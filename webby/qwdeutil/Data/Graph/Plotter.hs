module Data.Graph.Plotter
  where

import Data.List (genericLength, sort)
import Text.Printf (printf)

data Plot = Plot {
  plotData :: [PlotData]
  , xAxis :: Axis
  , yAxis :: Axis
  } deriving (Eq, Show)

data PlotData = PlotData {
  yData :: [Double]
  , yTicks :: [Int]
  , xTicks :: [Int]
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
getPlot :: Int -> Int -> Int -> [String] -> [[Double]] -> Plot
getPlot numLabels pxWidth pxHeight xli yli
  | or [null xli] = let axis = Axis 0 0 0 0 [] [] in Plot [(PlotData [] [] [])] axis axis
  | otherwise = Plot {
      plotData = map (\(orig, lam) -> PlotData orig lam (mapToXticks (length lam) pxWidth)) (zip yli yTickies)
      , xAxis = Axis { x1 = axisWidth, x2 = axisWidth, y2 = 0, y1 = pxHeight - fontHeight, labelPoints = xAxisLabelPoints, labels = xLabels }
      , yAxis = Axis { x1 = axisWidth, x2 = pxWidth, y2 = pxHeight - fontHeight, y1 = pxHeight - fontHeight, labelPoints = yAxisLabelPoints, labels = yLabels }

    } where
      yLabels = map (printf "%.2f") $ makeYlabels ((sort . concat) $ yli) numLabels minL
      minL = minimum $ map minimum yli
      maxL = maximum $ map maximum yli
      yTickies = map (map ((\x -> subtract x (sph - fontHeight)) . round . (* yValToPxRatio) . (subtract (minL)))) (yli) :: [[Int]]
      --yTickies = [[1..10]] :: [[Int]]
      xLabels = makeXlabels xli numLabels --each (((fromIntegral . length) xli :: Double) / fromIntegral numLabels) xli
      yValToPxRatio = (fromIntegral (sph - fontHeight)) / (maxL - minL)
      xAxisLabelPoints = 
        let inc = floor $ (fromIntegral (pxWidth - axisWidth) :: Double) / ((fromIntegral . pred) numLabels) :: Int
        in take numLabels $ iterate (+ inc) axisWidth :: [Int]
      yAxisLabelPoints = 
        let inc = floor $ (fromIntegral (sph - fontHeight) :: Double) / ((fromIntegral . pred) numLabels) :: Int
        in take numLabels $ iterate (subtract inc) sph :: [Int]
      -- StartPoint
      sph = pxHeight - fontHeight
      --ticks = map makeYticks yTicksIn pxHeight

makeXlabels :: [String] -> Int -> [String]
makeXlabels [] _ = []
makeXlabels (x:[]) _ = [x]
makeXlabels (x:y:[]) _ = [x,y]
makeXlabels li' numLabels
  | (length li' <= numLabels) = li'
  | otherwise = each numLabels li'

makeYlabels :: [Double] -> Int -> Double -> [Double]
makeYlabels li numLabels minL
  | numLabels <= 0 = []
  | numLabels == 1 = [head li]
  | numLabels == 2 = head li : [last li]
  | otherwise = let inc = (interval) / ((fromIntegral . pred) numLabels) in take numLabels $ iterate (+ inc) minL
  where
    interval = maximum li - minimum li

makeYticks :: [Double] -> Int -> [Double]
makeYticks li height = map average groups
  where
      numGroups = ceiling $ ((fromIntegral . length) li :: Double) / (fromIntegral height)
      groups = grouper li (\l -> length l <= numGroups) [] []

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
