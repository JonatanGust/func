module Main where

import Haste
import Haste.Graphics.Canvas
import Othello

import Haste.DOM
import Haste.Events


import Haste.Prim
import Haste.Foreign
import Data.IORef

import Pages

type Square  = [Point]
type State = [Square]



--renderAll :: Othello -> IO ()

brickSizeInt :: Int
brickSizeInt = 100

--brickSizeDouble :: Double
--brickSizeDouble = fromIntegral brickSizeInt

brickSize = 100

decidePlayer :: Othello -> Brick -> Brick
decidePlayer o Black = if canPM o White
                        then White
                        else Black
decidePlayer o White = if canPM o Black
                        then Black
                        else White
main = do
          canvas <- (mkCanvas "white")
          column documentBody [canvas]
          Just can <- fromElem (canvas)

          render can (drawBoard 0 0 "green" )
          ot <- newIORef (emptyO, Black)
          drawAll can emptyO
--          mapM (renderOnTop can) $ renderAll


          canvas `onEvent` Click $ \mouse -> do
                         (oc,p) <- readIORef ot
                         let (x,y)  = mouseCoords mouse
                             pos    = (fromIntegral (x), fromIntegral (y))
                             cX = x `div` brickSizeInt--(toInteger brickSize)
                             cY = y `div` brickSizeInt--(toInteger brickSize)
                             (b,on) = tryPB oc (cX,cY) p

                         if b
                            then
                                do writeIORef ot (on,decidePlayer on p)
                                   mapM_ (renderOnTop can) $ renderAll on
                            else
                                do mapM_ (renderOnTop can) $ renderAll on


--                         renderOnTop can (drawSquare (fst pos) (snd pos) "white" )


            where
          renderAll o = map (renderSquare) $ (concat (rows o)) `zip` posList
          drawAll can o = mapM (renderOnTop can) $ renderAll o

renderSquare :: (Maybe Brick, (Pos)) -> Picture ()
renderSquare (b,(x,y)) | b == (Just Black) = (drawCircle (doublex*brickSize) (doubley*brickSize) "black" )
                       | b == (Just White) = (drawCircle (doublex*brickSize) (doubley*brickSize) "white" )
                       | otherwise         = (drawSquare (doublex*brickSize) (doubley*brickSize) "green" )
                       where
                           doublex = fromIntegral x
                           doubley = fromIntegral y


--step :: State -> State
--step bs = [ ps | _:ps <- bs ]
--
--
--animate :: Canvas -> IORef State -> IO ()
--animate can state = do
--    squares <- readIORef state
----    writeIORef state $ step squares
--    render can $ mapM_ drawSquare squares
--    setTimer (Once 20) $ animate can state
--    return ()
--


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
squareShape x y = rect ((x, y)) (((x+brickSize), (y+brickSize)))

boardShape :: Double -> Double -> Shape ()
boardShape x y = rect ((x, y)) (x+(brickSize*8), (y+(brickSize*8)))

circleShape :: Double -> Double -> Shape ()
circleShape x y = circle (x+(brickSize/2),y+(brickSize/2)) ((brickSize - (brickSize / 10)) /2)

drawCircle :: Double -> Double -> String -> Picture ()
drawCircle x y clr = do
     color (getRGB "green") $ fill $ squareShape x y
     stroke $ squareShape x y
     color (getRGB clr) $ fill $ circleShape x y
     stroke $ circleShape x y

drawBoard :: Double -> Double -> String -> Picture ()
drawBoard x y clr = do
    color (getRGB clr) $ fill $ squareShape x y
    stroke $ squareShape x y

drawSquare :: Double -> Double -> String -> Picture ()
drawSquare x y clr = do
    color (getRGB clr) $ fill $ squareShape x y
    stroke $ squareShape x y


getRGB clr | clr == "green" = (RGB 0   255 0)
           | clr == "black" = (RGB 0   0   0)
           | clr == "white" = (RGB 255 255 255)

mkCanvas :: String -> IO Elem
mkCanvas s =
    newElem "canvas" `with` [style "border"          =: "1px solid black",
                             style "backgroundColor" =: s,
                             prop "width"            =: (show $8*brickSize),--show width,
                             prop "height"           =: (show $8*brickSize)]--show height]
