{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import System.Random
import Miso
import Miso.String
import Miso.Util (now)
import qualified Data.Map as M

-- -----------------------------
-- app
-- -----------------------------

main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp
    model  = initialModel
    update = updateModel
    view   = mainView
    events = defaultEvents
    mountPoint = Nothing
    subs   = [ every tick Time ]

-- -----------------------------
-- constants
-- -----------------------------

(gameWidth, gameHeight) = (480, 480)
(planeWidth, planeHeight) = (60, 35)
planeXPos = 70 -- gameWidth / 2 - 100
tick = 50000 -- 50ms
bgScrollVOffset = 50 / 1000 -- 50px / 1000ms
timeBetweenPillars = 1.6
gapToPlaneRatio = 3.5

randomHeight :: IO Double
randomHeight =
  let
    gapHeight = planeHeight * gapToPlaneRatio
    minHeight = gameHeight / 8
    maxHeight = gameHeight - minHeight - gapHeight
  in
  randomRIO (minHeight, maxHeight)

-- -----------------------------
-- model
-- -----------------------------

data PillarVPos
  = Top
  | Bottom
  deriving (Show, Eq)


data Pillar = Pillar
  { x :: Double
  , y :: Double
  , height :: Int
  , passed :: Bool
  , vPos :: PillarVPos
  } deriving (Show, Eq)

data Model = Model
  { counter :: !Int
  , time :: !Double
  , delta :: !Double
  , bgXPos :: !Double
  , planeYPos :: !Double
  , score :: !Int
  , pillars :: [Pillar]
  , timeToPillar :: !Double
  } deriving (Show, Eq)

initialModel :: Model
initialModel = Model
  { counter = 10
  , time = 0
  , delta = 0
  , bgXPos = 0
  , planeYPos = (gameHeight - planeHeight) / 2 -- center vertically
  , score = 0
  , pillars = mempty
  , timeToPillar = timeBetweenPillars
  }

-- -----------------------------
-- update
-- -----------------------------

data Action
  = SayHello
  | NoOp
  | Time !Double
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel SayHello model =
  let newCount = counter model + 1
      newModel = model { counter = newCount }
  in
    newModel <# do
      putStrLn $ "Hello " <> show newCount
      pure NoOp

updateModel NoOp model = noEff model

updateModel (Time newTime) model = noEff newModel
  where
    newModel = model
      { delta = newTime - time model -- ca. 50ms == tick
      , time = newTime
      , bgXPos =
        if bgXPos model >= gameWidth
          then 0
          else bgXPos model + delta model * bgScrollVOffset
      , planeYPos = planeYPos model + sin (bgXPos model / 20)
      }

-- -----------------------------
-- view
-- -----------------------------

mainView :: Model -> View Action
mainView m@Model{..} =
  div_
    []
    [
       button_ [ onClick SayHello ] [ text "Say hello" ]
     , h3_ [] [ text (ms $ "clicked " ++ show counter) ]
     , h3_ [] [ text (ms $ "time " ++ show time) ]
     , h3_ [] [ text (ms $ "delta " ++ show delta) ]
     , h3_ [] [ text (ms $ "bgXPos " ++ show bgXPos) ]
     , div_
        [ style_ wrapperStyle ]
        -- ^ container to wrap all game elements
        [ bgView $ negate bgXPos
        -- ^ left hand part of scrollable background image
        , bgView $ gameWidth - bgXPos
        -- ^ right-hand part of scrollable background image
        , planeView
        , headlineView
        , scoreView
        ]
     , div_ [] $ Prelude.map pillarView pillars
    ]
  where

    pillarView :: Pillar -> View Action
    pillarView p@Pillar{..} =
      let
        imgName :: String
        imgName = if vPos == Top
                    then "topRock"
                    else "bottomRock"
      in
      img_
        [ src_ $ ms $ "images/" ++ imgName ++ ".png"
        , style_ $
            M.fromList
              [ ("display", "block")
              , ("position", "absolute")
              , ("background-color", "green")
              , ("x", ms $ show x)
              , ("y", ms $ show y)
              ]
        ]
        []

    headlineView :: View Action
    headlineView =
      let width = 250
          xPos = (gameWidth - width) / 2
      in
      img_
        [ src_ "images/textGetReady.png"
        , style_ $
            M.fromList
              [ ("display", "block")
              , ("width", px width)
              , ("height", px 46)
              , ("position", "absolute")
              , ("background-color", "grey")
              , ("transform", ms $ "matrix(1,0,0,1," ++ show xPos ++ ",150)")
              ]
        ]
        []

    scoreView :: View Action
    scoreView =
      let scoreOutlineHexColor :: String
          scoreOutlineHexColor = "#005000"
      in
      p_
        [ style_ $
            M.fromList
              [ ("font-size", px 50)
              , ("text-shadow",
                    ms $ "-1px 0 "
                          ++ scoreOutlineHexColor
                          ++ ", 0 1px "
                          ++ scoreOutlineHexColor
                          ++ ", 1px 0 "
                          ++ scoreOutlineHexColor
                          ++ ", 0 -1px "
                          ++ scoreOutlineHexColor
                          )
              , ("color", "#32a032")
              , ("font-weight", "bold")
              , ("text-align", "center")
              , ("position", "absolute")
              , ("y", "70")
              , ("width", "100%")
              ]
        ]
        [ text $ ms $ show score]

    planeView :: View Action
    planeView =
      img_
        [ src_ "images/plane.gif"
        , style_ $
            M.fromList
              [ ("display", "block")
              , ("width", px planeWidth)
              , ("height", px planeHeight)
              , ("position", "absolute")
              , ("background-color", "grey")
              , ("transform", ms $ "matrix(1,0,0,1," ++ show planeXPos ++ ", " ++ show planeYPos ++ ")")
              ]
        ]
        []

    bgView :: Double -> View Action
    bgView xPos =
      img_
          [ src_ "images/background.png"
          , style_ $
              M.fromList
                [ ("display", "block")
                , ("width", px gameWidth)
                , ("height", px gameHeight)
                , ("position", "absolute")
                , ("background-color", "darkgrey")
                , ("transform", ms $ "matrix(1,0,0,1," ++ show xPos ++ ",0)")
                ]
          ]
          []

    wrapperStyle :: M.Map MisoString MisoString
    wrapperStyle =
      M.fromList
        [ ("width", px gameWidth)
        , ("height", px gameHeight)
        , ("overflow", "hidden")
        , ("position", "relative")
        ]

-- -----------------------------
-- helper
-- -----------------------------

-- | Utility for periodic tick subscriptions
every :: Int -> (Double -> action) -> Sub action
every n f sink = void . forkIO . forever $ do
  threadDelay n
  sink =<< f <$> now

-- | Creates a pixel value
px :: Show a => a -> MisoString
px v =
  ms $ show v ++ "px"
