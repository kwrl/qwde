{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Arrow
import qualified Common as C
import Data.Proxy

import qualified Data.Map as M

import Miso
import Miso.String

import Touch

main :: IO ()
main = miso $ \currentURI -> App
  { model = C.Model currentURI False "[1, 2]" (0,0) "20,20 40,25 60,40 80,120 120,140 200,180"
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
updateModel C.ShowRandomDefault m@C.Model{..} = noEff m { {-C.randomNumbers = "[2]", -}C.mainPlot = "0,40 40,40 40,80 80,80 80,120 120,120 120,160" }
updateModel C.NoOp m = noEff m
updateModel (C.HandleTouch (TouchEvent touch)) model =
  model <# do
    putStrLn "Touch did move"
    print touch
    return $ C.HandleMouse $ trunc . page $ touch
updateModel (C.HandleMouse newCoords) model =
  noEff model { C.mouseCords = newCoords }

trunc = truncate *** truncate
