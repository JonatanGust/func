module Main where

import Haste
import Haste.Graphics.Canvas
import Othello

import Haste.DOM
import Haste.Events

import Data.IORef

import Pages

main = do
            canvas <- (mkCanvas "white")
            column documentBody [canvas]
            Just can <- fromElem (canvas)
            render can (drawSquare 1 1 "black" )


canvasOthello :: Othello -> Canvas
canvasOthello o = undefined


blackBrick = brick "black"
whiteBrick = brick "white"
greenBrick = brick "green"

brick :: String -> IO Elem
brick s = mkCanvas s

mkButton :: String -> IO Elem
mkButton label =
  newElem "input" `with` [attr "type"  =: "button",
                          attr "value" =: label]

squareShape :: Double -> Double -> Shape ()
squareShape x y = rect ((x, y)) (((x+20), (y+20)))


drawSquare :: Double -> Double -> String -> Picture ()
drawSquare x y clr = do
    color (getRGB "white") $ fill $ squareShape x y
    stroke $ squareShape x y



--getRGB :: String -> (RGB x y z)
getRGB clr | clr == "green" = (RGB 0   255 0)
           | clr == "black" = (RGB 0   0   0)
           | clr == "white" = (RGB 255 255 255)

mkCanvas :: String -> IO Elem
mkCanvas s =
    newElem "canvas" `with` [style "border"          =: "1px solid black",
                             style "backgroundColor" =: "white",
                             prop "width"            =: "400",--show width,
                             prop "height"           =: "400"]--show height]
