{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Common as C
import qualified Data.Graph.Plotter as P

import Touch

import Data.Colour (Colour)
import Data.Colour.Names
import Control.Arrow
import Data.Proxy
import qualified Data.Map as M
import Miso hiding (defaultOptions, map, length)
import Miso.String hiding (map, length, take, zip)
import System.Random (randomRIO)
import JavaScript.Web.XMLHttpRequest
import Data.Aeson
import Data.Aeson.Types

main :: IO ()
main = miso $ \currentURI -> App
  { model = C.Model currentURI False "[1, 2]" (0,0)
      (P.getPlot 10 C.plotWidth C.plotHeight (map show ([1..10] :: [Int])) ([[1..10]] :: [[Double]]) ([P.PlotLegend "" C.defaultColor]))
      (P.getPlot 10 C.plotWidth C.plotHeight (map show ([1..10] :: [Int])) ([[1..10]] :: [[Double]]) ([P.PlotLegend "" C.defaultColor]))
  , view = viewModel
  , ..
    }
      where
        initialAction = C.NoOp
        mountPoint = Nothing
        update = updateModel
        --events = defaultEvents
        --subs = [ uriSub C.HandleURI ]
        events = M.insert (pack "mousemove") False $
                 M.insert (pack "touchstart") False $
                 M.insert (pack "touchmove") False defaultEvents
        subs = [ uriSub C.HandleURI, (mouseSub C.HandleMouse) ]
        viewModel m =
          case runRoute (Proxy :: Proxy C.ClientRoutes) C.handlers C.uri m of
            Left _ -> C.the404 m
            Right v -> v

instance FromJSON C.QwdeRandom where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance FromJSON C.QwdeSma where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

getQwdeRandom :: IO C.QwdeRandom
getQwdeRandom = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String C.QwdeRandom of
    Left s -> error s
    Right j -> pure j
  where
    req = Request { reqMethod = GET
                  , reqURI = pack "http://localhost:8080/random"
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }

getQwdeSma :: IO C.QwdeSma
getQwdeSma = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String C.QwdeSma of
    Left s -> error s
    Right j -> pure j
  where
    req = Request { reqMethod = GET
                  , reqURI = pack "http://localhost:8080/sma/twtr/20150102?toDate=20170301"
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }

updateModel :: C.Action -> C.Model -> Effect C.Action C.Model
updateModel (C.HandleURI u) m = m { C.uri = u } <# do
  pure C.NoOp
updateModel (C.ChangeURI u) m = m { C.navMenuOpen = False } <# do
  pushURI u
  pure C.NoOp
updateModel C.Alert m@C.Model{..} = m <# do
  alert $ pack (show uri)
  pure C.NoOp
updateModel C.ToggleNavMenu m@C.Model{..} = m { C.navMenuOpen = not navMenuOpen } <# do
  pure C.NoOp
updateModel C.GetRandom m@C.Model{..} = m <# do
  C.SetRandom <$> getQwdeRandom
updateModel (C.SetRandom apiData) m@C.Model{..} = noEff m { C.randomPlot = P.getPlot 10 C.plotWidth (C.plotHeight - 200)
  (take (length $ C.numbers apiData) $ map show ([1..] :: [Int]))
  ((((map P.yData) . P.plotData) randomPlot) ++ [C.numbers apiData])
  ([P.PlotLegend "random" C.defaultColor])
   }
updateModel C.GetSma m@C.Model{..} = m <# do
  C.SetSma <$> getQwdeSma
updateModel (C.SetSma apiData) m@C.Model{..} = noEff m { C.smaPlot = P.getPlot 10 C.plotWidth (C.plotHeight - 200)
  (take (length $ C.prices apiData) $ map show ([1..] :: [Int]))
  ([C.prices apiData] ++ (C.sma apiData))
  ([P.PlotLegend "sma" C.defaultColor ] ++ (map (\c -> P.PlotLegend "+2" c) $ take (length $ C.prices apiData) colorList))
   }
updateModel C.NoOp m = noEff m
updateModel (C.HandleTouch (TouchEvent touch)) model =
  model <# do
    putStrLn "Touch did move"
    print touch
    return $ C.HandleMouse $ trunc . page $ touch
updateModel (C.HandleMouse newCoords) model =
  noEff model { C.mouseCords = newCoords }

trunc :: (Double, Double) -> (Int, Int)
trunc = truncate *** truncate


colorList :: [Colour Double]
colorList = [
  aliceblue
  , antiquewhite
  , aqua
  , aquamarine
  , azure
  , beige
  , bisque
  , blanchedalmond
  , blue
  , blueviolet
  , brown
  , burlywood
  , cadetblue
  , chartreuse
  , chocolate
  , coral
  , cornflowerblue
  , cornsilk
  , crimson
  , cyan
  , darkblue
  , darkcyan
  , darkgoldenrod
  , darkgray
  , darkgreen
  , darkgrey
  , darkkhaki
  , darkmagenta
  , darkolivegreen
  , darkorange
  , darkorchid
  , darkred
  , darksalmon
  , darkseagreen
  , darkslateblue
  , darkslategray
  , darkslategrey
  , darkturquoise
  , darkviolet
  , deeppink
  , deepskyblue
  , dimgray
  , dimgrey
  , dodgerblue
  , firebrick
  , floralwhite
  , forestgreen
  , fuchsia
  , gainsboro
  , ghostwhite
  , gold
  , goldenrod
  , gray
  , grey
  , green
  , greenyellow
  , honeydew
  , hotpink
  , indianred
  , indigo
  , ivory
  , khaki
  , lavender
  , lavenderblush
  , lawngreen
  , lemonchiffon
  , lightblue
  , lightcoral
  , lightcyan
  , lightgoldenrodyellow
  , lightgray
  , lightgreen
  , lightgrey
  , lightpink
  , lightsalmon
  , lightseagreen
  , lightskyblue
  , lightslategray
  , lightslategrey
  , lightsteelblue
  , lightyellow
  , lime
  , limegreen
  , linen
  , magenta
  , maroon
  , mediumaquamarine
  , mediumblue
  , mediumorchid
  , mediumpurple
  , mediumseagreen
  , mediumslateblue
  , mediumspringgreen
  , mediumturquoise
  , mediumvioletred
  , midnightblue
  , mintcream
  , mistyrose
  , moccasin
  , navajowhite
  , navy
  , oldlace
  , olive
  , olivedrab
  , orange
  , orangered
  , orchid
  , palegoldenrod
  , palegreen
  , paleturquoise
  , palevioletred
  , papayawhip
  , peachpuff
  , peru
  , pink
  , plum
  , powderblue
  , purple
  , red
  , rosybrown
  , royalblue
  , saddlebrown
  , salmon
  , sandybrown
  , seagreen
  , seashell
  , sienna
  , silver
  , skyblue
  , slateblue
  , slategray
  , slategrey
  , snow
  , springgreen
  , steelblue
  , Data.Colour.Names.tan
  , teal
  , thistle
  , tomato
  , turquoise
  , violet
  , wheat
  , white
  , whitesmoke
  , yellow
  , yellowgreen
  ]

