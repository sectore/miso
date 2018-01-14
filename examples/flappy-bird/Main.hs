{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Miso
import Miso.String

type Model = Int

data Action
  = SayHello
  | NoOp
  deriving (Show, Eq)

main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp
    model  = 0
    update = updateModel
    view   = mainView
    events = defaultEvents
    mountPoint = Nothing
    subs   = []

updateModel :: Action -> Model -> Effect Action Model
updateModel SayHello count =
  let newCount = count + 1
  in
    newCount <# do
      putStrLn $ "Hello " <> show newCount
      pure NoOp
updateModel NoOp m = noEff m

mainView :: Model -> View Action
mainView model =
  div_
    []
    [
       button_ [ onClick SayHello ] [ text "Say hello" ]
     , text (ms $ show model)
     ]
