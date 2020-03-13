{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE CPP                        #-}
module Main where

import qualified            Common as C
import qualified            Data.Graph.Plotter as P
import           Data.Aeson
import           Data.Proxy
import           Data.Text                            (Text)
import           GHC.Generics
import qualified Lucid                                as L
import           Lucid.Base
import           Network.HTTP.Types hiding (Header)
import Network.URI (parseURI)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Application.Static
import           Network.Wai.Middleware.RequestLogger
import           Servant
import qualified System.IO                            as IO

import           Miso
import           Miso.String hiding (map)

main :: IO ()
main = do
  IO.hPutStrLn IO.stderr "Running on port 8081..."
  run 8081 $ logStdout (compress app)
    where
      compress = gzip def { gzipFiles = GzipCompress }

initialModel :: C.Model
initialModel = C.Model uri False (0,0) (P.getPlot 10 C.plotWidth C.plotHeight (map show ([1..10] :: [Int])) [[1..10]] [P.PlotLegend "" C.defaultColor])
  (P.getPlot 10 C.plotWidth C.plotHeight (map show ([1..10] :: [Int])) [[1..10]] [P.PlotLegend "" C.defaultColor])
  where
    uri = case parseURI "http://qwde.no" of
            Just n -> n
            Nothing -> error "misunderstood API?"
 

app :: Application
app = serve (Proxy @ API) (static :<|> serverHandlers :<|> pure misoManifest :<|> Tagged handle404)
  where
    static = serveDirectoryWith (defaultWebAppSettings "static")

-- | Wrapper for setting HTML doctype and header
newtype Wrapper a = Wrapper a
  deriving (Show, Eq)

-- | Convert client side routes into server-side web handlers
type ServerRoutes = ToServerRoutes C.ClientRoutes Wrapper C.Action

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
      renderBS $ toHtml $ Wrapper $ C.the404 C.Model { C.uri = C.goHome, C.navMenuOpen = False, C.mouseCords = (0,0)
        , C.randomPlot = C.randomPlot initialModel
        , C.smaPlot = C.smaPlot initialModel
        }

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
          L.style_ ((pack ("body{font-family:'Open Sans', sans-serif;}.graph .labels.x-labels{text-anchor:middle;}.graph .labels.y-labels{text-anchor:end;}.graph{height:")
            <> (pack $ show C.plotHeight)
            <> (pack "px;width:")
            <> (pack $ show C.plotWidth) <> (pack "px;}.graph .grid{stroke:#ccc;stroke-dasharray:0;stroke-width:1;}.labels{font-size:")
            <> (pack $ show P.fontHeight) <> (pack "px;}.label-title{font-weight:bold;text-transform:uppercase;font-size:12px;fill:black;}.data{fill:red;stroke-width:1;}")))
          cssRef animateRef
          cssRef bulmaRef
          cssRef fontAwesomeRef
          jsRef "/static/buttons.js"
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
--        Handler (Wrapper (View C.Action))
--   :<|> Handler (Wrapper (View C.Action))
--   :<|> Handler (Wrapper (View C.Action))
-- serverHandlers = examplesHandler
--   :<|> docsHandler
--   :<|> homeHandler
--      where
--        send f u = pure $ Wrapper $ f C.Model {uri = u, navMenuOpen = False}
--        homeHandler = send C.home C.goHome
--        examplesHandler = send examples goExamples
--        docsHandler  = send docs goDocs

serverHandlers ::
       Handler (Wrapper (View C.Action))
  :<|> Handler (Wrapper (View C.Action))
  :<|> Handler (Wrapper (View C.Action))
serverHandlers = homeHandler
  :<|> smaHandler
  :<|> homeHandler
     where
       send f u = pure $ Wrapper $ f initialModel
       homeHandler = send C.home C.goHome
       smaHandler = send C.smaPage C.goSma
