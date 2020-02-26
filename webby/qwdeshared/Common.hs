{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
module Common where

import           Data.Bool
import qualified Data.Map    as M
import           Data.Proxy
import           Servant.API
#ifdef __GHCJS__
import           Servant.Links (linkURI)
#else
import           Servant.Utils.Links (linkURI)
#endif
import qualified Data.Graph.Plotter as P

import           Miso
import           Miso.String hiding (unwords)
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Attribute as SVGA
import           Touch


githubUrl :: MisoString
githubUrl = "https://github.com/kwrl/qwde"

data Model = Model { 
  uri :: URI, navMenuOpen :: Bool
  , randomNumbers  :: String
  , mouseCords :: (Int, Int) 
  , plot :: P.Plot
  } deriving (Eq, Show)

data Action
  = Alert
  | ChangeURI URI
  | HandleURI URI
  | ToggleNavMenu
  | ShowRandomDefault
  | HandleTouch TouchEvent
  | HandleMouse (Int, Int)
  | NoOp
  deriving (Show, Eq)

-- | Router
type ClientRoutes = Examples
  :<|> Docs
  :<|> Home

-- | Handlers
handlers :: (Model -> View Action) :<|> ((Model -> View Action) :<|> (Model -> View Action))
handlers = home :<|> home :<|> home
-- examples
--   :<|> docs
--   :<|> home


-- | Client Routes
type Examples  = "examples" :> View Action
type Docs      = "docs" :> View Action
type Home      = View Action

plotWidth :: Int
plotWidth = 800
plotHeight :: Int
plotHeight = 500
numLabels :: Int
numLabels = 10

misoSrc :: MisoString
misoSrc = pack "/static/Creative-Tail-Animal-cat.svg"

makeAxis :: Bool -> P.Axis -> View Action
makeAxis isX P.Axis{..} = let letter = if isX then "x" else "y" 
  in SVG.g_ [ class_ (toMisoString ("grid " ++ letter ++ "-grid")) ] [
    SVG.line_ [ SVGA.x1_ $ ms x1
              , SVGA.x2_ $ ms x2
              , SVGA.y1_ $ ms y1
              , SVGA.y2_ $ ms y2
    ] [] ]

makeLabelpoints :: Bool -> P.Axis -> View Action
makeLabelpoints isX P.Axis{..} = let letter = if isX then "x" else "y" 
  in SVG.g_ [ class_ (toMisoString ("labels " ++ letter ++ "-labels")) ] $ labelsFunc labelPoints labels
  where
    recurring = if isX then SVGA.y_ $ ms y1 else SVGA.x_ $ ms x1
    newPoint p = if isX then SVGA.x_ $ ms p else SVGA.y_ $ ms p
    -- TODO: zip
    labelsFunc (x:xs) (y:ys) = [SVG.text_ [ recurring , newPoint x ] [(text . toMisoString) y] ] ++ labelsFunc xs ys
    labelsFunc [] [] = []
    labelsFunc [] (_:_) = []
    labelsFunc (_:_) [] = []

-- makeLine :: [(Int,Int)] -> View Action
-- makeLine points = a

home :: Model -> View Action
home m@Model{..} = template header content m
  where
    header = div_ [ class_  "animated fadeIn" ] [
        a_ [ href_ githubUrl ] [
           img_ [ width_ "100"
                , class_ "animated bounceInDown"
                , src_ misoSrc
                , alt_ "miso logo"
                ]
           ]
        , h1_ [ class_  "title animated pulse"
              , style_ $ M.fromList [(pack "font-size", pack "82px")
                                    ,(pack "font-weight", pack "100")
                                    ]
        ] [ text "qwde" ]
       , h2_ [ class_ "subtitle animated pulse" ] [
        text "making lots of "
        , a_ [ href_ "https://medium.com/startup-leadership/the-best-way-to-learn-something-make-a-lot-of-pots-7f4aa97e1d3a"
             , rel_ "noopener"
             , target_ "_blank"][
            strong_ [] [text "pots" ]]
        , text  " for fun."
        ]
      ]
    x = fst mouseCords
    y = (snd mouseCords) - 10
    content = div_ [ class_  "content has-text-centered" ] ([
        p_ [] [
          text $ "hello, world!\n"
        ]
      , p_ [ id_ $ toMisoString randomNumbers ] [
           text $ "hello"
          , text $ toMisoString randomNumbers
       ]
      , div_ [ ] [
      --   SVG.svg_ [ SVGA.viewBox_ "0 0 500 100" , (SVGA.style_ chartCssOneLine) ] [ 
      -- SVG.g_ [] [
      --   SVG.ellipse_ [ SVGA.cx_ $ ms x
      --           , SVGA.cy_ $ ms y
      --           , style_ svgStyle
      --           , SVGA.rx_ "100"
      --           , SVGA.ry_ "100"
      --           ] [ ]
      --         , SVG.text_ [ SVGA.x_ $ ms x
      --                 , SVGA.y_ $ ms y
      --                 ] [ text $ ms $ show (x,y) ]
      --   , SVG.polyline_ [ SVGA.fill_ "none", SVGA.stroke_ "black", SVGA.strokeWidth_ "3", SVGA.points_ $ toMisoString mainPlot ] []
      --     ] ]
        SVG.svg_ [ {-SVGA.viewBox_ "0 0 100 100",-} class_ "graph" ] [
          makeAxis True (P.xAxis plot)
          , makeAxis False (P.yAxis plot)
          , makeLabelpoints True (P.xAxis plot)
          , makeLabelpoints False (P.yAxis plot)
          ]
        , button_ [ id_ "dome", onClick ShowRandomDefault ] [ text "doit" ]
        , div_ [ id_ "myDiv" ] []
        --, script_ [] [ text $ toMisoString $ "doSimpleTrace(" ++ (show $ Prelude.head randomNumbers) ++ ")" ]
     ]]) 

chartCss :: M.Map MisoString MisoString
chartCss = M.insert "background" "white" $
  M.insert "width" "500px" $
  M.insert "height" "200px" $
  M.insert "border-left" "1px dotted #555" $
  M.insert "padding" "20px 20px 20px 0" M.empty

chartCssOneLine :: MisoString
chartCssOneLine = "background: white; width: 400px; height: 400px; border-left: 1px dotted #555; border-bottom: 1px dotted #555; padding: 20px 20px 20px 0;"

template :: View Action -> View Action -> Model -> View Action
template header content Model{..} =
  div_ [ ] [
  hero header uri navMenuOpen
  , content 
  , middle
  , footer
  ]


middle :: View Action
middle =
  section_ [class_ "hero" ] [
    div_ [class_ "hero-body"] [
      div_ [class_ "container"] [
        nav_ [class_ "columns"] [
               a_ [ class_ "column has-text-centered"
                   , href_ "https://rawgit.com/krausest/js-framework-benchmark/master/webdriver-ts-results/table.html"
                   , target_ "_blank"
                   , rel_ "noopener"
                   ] [
                  span_ [class_   "icon is-large"] [
                      i_ [class_   "fa fa-flash"] [ ]
                      ],
                  p_ [class_   "title is-4"] [
                     strong_ [] [ text  "Fast"]
                  ],
                  p_ [class_   "subtitle"] [
                        text  "Virtual DOM diffing algorithm"
                      ]
                  ]

              , a_ [ class_   "column has-text-centered"
                   , href_  "https://en.wikipedia.org/wiki/Isomorphic_JavaScript"
                   , target_ "_blank"
                   , rel_ "noopener"
                   ] [
                  span_ [class_   "icon is-large"] [
                      i_ [class_   "fa fa-line-chart"] [ ]
                      ],
                  p_ [class_   "title is-4"] [
                     strong_ [] [ text  "Isomorphic"]
                  ],
                  p_ [class_   "subtitle"]
                      [ text  "Seamless web experience" ]
                  ],
                  a_ [ class_   "column has-text-centered"
                     , target_  "_blank"
                     , href_  "http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html"
                     , rel_ "noopener"
                     ] [
                    span_ [class_  "icon is-large"] [
                       i_ [class_  "fa fa-gears"] [ ]
                    ], p_ [class_  "title is-4"] [
                        strong_ [] [ text  "Concurrent" ]
                       ],
                      p_ [class_   "subtitle"] [
                        text  "Type-safe and polymorphic, GHC Haskell"
                       ]
                    ],
                  a_ [class_ "column has-text-centered"
                     , href_  "https://github.com/ghcjs/ghcjs/blob/master/doc/foreign-function-interface.md"
                     , rel_ "noopener"
                     , target_  "_blank"
                     ] [
                    span_ [class_   "icon is-large"] [
                       i_ [class_   "fa fa-code-fork"] [ ]
                    ], p_ [class_   "title is-4"] [
                        strong_ [] [ text  "Interoperable" ]
                       ],
                      p_ [class_   "subtitle"] [
                        text  "via the GHCJS FFI"
                        ]
                    ]
              ]
          ]
        ]
      ]

cols :: View action
cols = section_[][div_ [ class_  "container" ] [
  div_ [class_  "columns" ] [
   div_ [ class_  "column" ] [
     h1_ [class_  "title" ] [
       span_ [class_"icon is-large"] [i_[class_"fa fa-flash"][]]
     , text  "Fast"
     ]
   , h2_ [class_  "subtitle" ] [
       text  "Mutable virtual dom implementation"
      ]
   ]
   , div_ [ class_  "column" ] [
     text  "Second column"
   ]
   , div_ [ class_  "column" ] [
      text  "Third column"
   ]
   , div_ [ class_  "column" ] [
      text  "Fourth column"
    ]
  ]]]

the404 :: Model -> View Action
the404 = template header content
  where
    header = div_ [] [ a_ [ href_ githubUrl ] [ img_ [
           width_  "100"
         , class_  "animated bounceOutUp"
         , src_ misoSrc
         , alt_ "miso logo"
         ]]
         , h1_ [ class_  "title"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
         ] [ text "404" ]
       , h2_ [ class_  "subtitle animated pulse" ] [
          text "No soup for you! "
          , a_ [ href_ "/", onPreventClick (ChangeURI goHome) ] [ text " - Go Home" ]
         ]
       ]
    content = p_ [] [text ":(" ]

-- | Links
goHome :: URI
  --, goExamples, goDocs  :: URI
( goHome ) =
  -- , goExamples, goDocs ) =
    ( linkURI (safeLink routes homeProxy)
    -- , linkURI (safeLink routes examplesProxy)
    -- , linkURI (safeLink routes docsProxy)
    )

homeProxy :: Proxy Home
homeProxy = Proxy
-- examplesProxy :: Proxy Examples
-- examplesProxy = Proxy
-- docsProxy :: Proxy Docs
-- docsProxy = Proxy
routes :: Proxy ClientRoutes
routes = Proxy

-- | Hero
hero :: View Action -> URI -> Bool ->  View Action
hero content uri' navMenuOpen' =
  section_ [ class_  "hero is-warning is-bold has-text-centered" ] [
    div_ [ class_ "hero-head" ] [
     header_ [class_"nav"] [
      div_ [class_"container"] [
        div_ [class_"nav-left"][
          a_ [class_"nav-item"][]
          ],
        span_ [class_$ "nav-toggle " <> bool mempty "is-active" navMenuOpen'
              , onClick ToggleNavMenu
              ] [
          span_[][]
        , span_[][]
        , span_[][]
        ],
         div_ [ class_ $ "nav-right nav-menu " <> do  bool mempty "is-active" navMenuOpen'] [
          a_ [ class_ $ "nav-item " <> do  bool mempty "is-active" (uriPath uri' == uriPath goHome)
             , href_ "/", onPreventClick (ChangeURI goHome) ] [ text"Home" ]
          -- a_ [class_ $ "nav-item " <> do  bool mempty "is-active" (uriPath uri' == uriPath goExamples)
          --    , href_ "/examples", onPreventClick (ChangeURI goExamples)
          --    ] [ text"Examples" ],
          -- a_ [class_ $ "nav-item " <> do  bool mempty "is-active" (uriPath uri' == uriPath goDocs)
          --    , href_ "/docs", onPreventClick (ChangeURI goDocs)
          --    ] [ text"Docs" ]
          ]]]]
    , div_ [ class_  "hero-body" ] [
     div_ [ class_  "container" ] [
           content
         ]
     ]
  ]

onPreventClick :: Action -> Attribute Action
onPreventClick action =
  onWithOptions defaultOptions { preventDefault = True }
    "click" emptyDecoder (\() -> action)

-- | Footer
footer :: View action
footer =
  footer_ [ class_  "footer" ] [
    div_ [ class_  "container", style_ $ M.singleton "line-height" "0.7" ] [
      div_ [ class_  "content has-text-centered" ] [
         p_ [] [
         text "made using "
         , strong_ [] [ text "Miso" ]
         ,  text " by "
         ,  a_ [ href_  "https://github.com/dmjio/miso"
               , style_ $ M.singleton "color" "#363636"
               ]
              [ text "dmjio" ]
        , br_ []
         ,  text "The source code for this website is located "
                 , a_ [ href_  githubUrl
                      , style_ $ M.singleton "color" "#363636"
                      ] [  text" here."]
                 ]
         , p_ [] [ a_ [href_"https://bulma.io"] [ img_
                                                    [ src_ "static/made-with-bulma.png"
                                                    , alt_ "Made with Bulma"
                                                    , width_ "128"
                                                    , height_ "24"
                                                    ]
                                                ] ]
         , p_ [] ["cat-logo by Vektora kato"]
         , p_ [] [
           a_ [ href_ githubUrl ] [ span_ [ class_"icon is-large"]
                  [
                    i_ [ class_"fa fa-github"][ ]
                  ]
                                                        ]
           ]
        ]
      ]
    ]

svgStyle :: M.Map MisoString MisoString
svgStyle =
  M.fromList [
          ("fill", "yellow")
             , ("stroke", "purple")
             , ("stroke-width", "2")
             ]
