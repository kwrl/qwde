-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE TypeFamilies #-}
module Main where

-- | Miso framework import
import Miso
import Miso.String

import Control.Monad.IO.Class

-- | Type synonym for an application model
type Model = Int

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m
updateModel SayHelloWorld m = m <# do
  liftIO (putStrLn "Hello World") >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (ms x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]
