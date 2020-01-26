{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE CPP                        #-}
module Main where

import           Common
import           Data.Aeson
import           Data.Proxy
import           Data.Text                            (Text)
import           GHC.Generics
import qualified Lucid                                as L
import           Lucid.Base
import           Network.HTTP.Types hiding (Header)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Application.Static
import           Network.Wai.Middleware.RequestLogger
import           Servant
import qualified System.IO                            as IO

import           Miso
import           Miso.String

main :: IO ()
main = do
  IO.hPutStrLn IO.stderr "Running on port 8081..."
  run 8081 $ logStdout (compress app)
    where
      compress = gzip def { gzipFiles = GzipCompress }

app :: Application
app = serve (Proxy @ API) (static :<|> serverHandlers :<|> pure misoManifest :<|> Tagged handle404)
  where
    static = serveDirectoryWith (defaultWebAppSettings "static")

-- | Wrapper for setting HTML doctype and header
newtype Wrapper a = Wrapper a
  deriving (Show, Eq)

-- | Convert client side routes into server-side web handlers
type ServerRoutes = ToServerRoutes ClientRoutes Wrapper Action

-- | API type
type API = ("static" :> Raw)
  :<|> ServerRoutes
  :<|> ("manifest.json" :> Get '[JSON] Manifest)
  :<|> Raw

data Manifest
  = Manifest
  { name :: Text
  , short_name :: Text
  , start_url :: Text
  , display :: Text
  , theme_color :: Text
  , description :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Manifest

misoManifest :: Manifest
misoManifest =
  Manifest { name = "Haskell Miso"
           , short_name = "Miso"
           , start_url = "."
           , display = "standalone"
           , theme_color = "#00d1b2"
           , description = "A somehwat tasty Haskell front-end framework"
           }

handle404 :: Application
handle404 _ respond = respond $ responseLBS
    status404
    [("Content-Type", "text/html")] $
      renderBS $ toHtml $ Wrapper $ the404 Model { uri = goHome, navMenuOpen = False, randomNumbers = "[-1]", mouseCords = (0,0), mainPlot = "" }

superAdvancedScript :: MisoString
superAdvancedScript = "function doSimpleTrace(num){var trace1 = { x: [1, 2, 3, 4, 5, 6, 7], y: [num, num, num, 10, 15, 13, 17], type: 'scatter' }; var trace2 = { x: [1, 2, 3, 4], y: [16, 5, 11, 9], type: 'scatter' }; var data = [trace1, trace2]; Plotly.newPlot('myDiv', data); };"

instance L.ToHtml a => L.ToHtml (Wrapper a) where
  toHtmlRaw = L.toHtml
  toHtml (Wrapper x) = do
      L.doctype_
      L.html_ [ L.lang_ "en" ] $ do
        L.head_ $ do
          L.title_ "qwde"
          L.link_ [ L.rel_ "stylesheet"
                  , L.href_ "static/gh-fork-ribbon.min.css"
                  ]
          L.link_ [ L.rel_ "manifest"
                  , L.href_ "/manifest.json"
                  ]
          L.meta_ [ L.httpEquiv_ "content-type", L.content_ "text/html; charset=utf-8" ]
          L.meta_ [ L.charset_ "utf-8" ]
          L.meta_ [ L.name_ "theme-color", L.content_ "#00d1b2" ]
          L.meta_ [ L.httpEquiv_ "X-UA-Compatible"
                  , L.content_ "IE=edge"
                  ]
          L.meta_ [ L.name_ "viewport"
                  , L.content_ "width=device-width, initial-scale=1"
                  ]
          L.meta_ [ L.name_ "description"
                  , L.content_ "qwde is a work in progress"
                  ]
          L.style_ "body{font-family:'Open Sans', sans-serif;}.graph .labels.x-labels{text-anchor:middle;}.graph .labels.y-labels{text-anchor:end;}.graph{height:500px;width:800px;}.graph .grid{stroke:#ccc;stroke-dasharray:0;stroke-width:1;}.labels{font-size:13px;}.label-title{font-weight:bold;text-transform:uppercase;font-size:12px;fill:black;}.data{fill:red;stroke-width:1;}"
          cssRef animateRef
          cssRef bulmaRef
          cssRef fontAwesomeRef
          jsRef "/static/buttons.js"
          -- jsSyncRef "static/plotly-latest.min.js"
          -- L.script_ superAdvancedScript
          jsRef "/static/all.js"
        L.body_ (L.toHtml x)
          where
            jsRef href =
              L.with (L.script_ mempty)
                [ makeAttribute "src" href
                , makeAttribute "async" mempty
                , makeAttribute "defer" mempty
                ]
            jsSyncRef href =
              L.with (L.script_ mempty)
                [ makeAttribute "src" href
                ]
            cssRef href =
              L.with (L.link_ mempty) [
                  L.rel_ "stylesheet"
                , L.type_ "text/css"
                , L.href_ href
                ]

fontAwesomeRef :: MisoString
fontAwesomeRef = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"

animateRef :: MisoString
animateRef = "static/animate.min.css"

bulmaRef :: MisoString
bulmaRef = "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css"

-- serverHandlers ::
--        Handler (Wrapper (View Action))
--   :<|> Handler (Wrapper (View Action))
--   :<|> Handler (Wrapper (View Action))
-- serverHandlers = examplesHandler
--   :<|> docsHandler
--   :<|> homeHandler
--      where
--        send f u = pure $ Wrapper $ f Model {uri = u, navMenuOpen = False}
--        homeHandler = send home goHome
--        examplesHandler = send examples goExamples
--        docsHandler  = send docs goDocs

serverHandlers ::
       Handler (Wrapper (View Action))
  :<|> Handler (Wrapper (View Action))
  :<|> Handler (Wrapper (View Action))
serverHandlers = homeHandler
  :<|> homeHandler
  :<|> homeHandler
     where
       send f u = pure $ Wrapper $ f Model {uri = u, navMenuOpen = False, randomNumbers = "[5]", mouseCords = (0,0), mainPlot = "20,20 40,25 60,40 80,120 120,140 200,180"
 }
       homeHandler = send home goHome
