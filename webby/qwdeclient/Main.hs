{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Common as C
import qualified Data.Graph.Plotter as P

import Touch

import Control.Arrow
import Data.Proxy
import qualified Data.Map as M
import Miso hiding (defaultOptions, map)
import Miso.String hiding (map)
import System.Random (randomRIO)
import JavaScript.Web.XMLHttpRequest
import Data.Aeson
import Data.Aeson.Types

main :: IO ()
main = miso $ \currentURI -> App
  { model = C.Model currentURI False "[1, 2]" (0,0) (P.getPlot 10 C.plotWidth C.plotHeight ([1..10] :: [Double]) (map show [1..10]))
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

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,6)
  rs <- randomList (n-1)
  return (r:rs) 

instance FromJSON C.QwdeApiData where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo '_' }

getQwdeRandom :: IO C.QwdeApiData
getQwdeRandom = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String C.QwdeApiData of
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

fetchApiData :: IO C.QwdeApiData
fetchApiData = do
  nums <- (Prelude.map fromIntegral) <$> randomList 100
  return $ C.QwdeApiData nums


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
updateModel C.GetData m@C.Model{..} = m <# do
  --C.SetData <$> fetchApiData
  C.SetData <$> getQwdeRandom
updateModel (C.SetData apiData) m@C.Model{..} = noEff m { C.plot = P.getPlot 10 C.plotWidth (C.plotHeight - 200) (C.numbers apiData) (["abc", "def"] :: [String]) } 
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
