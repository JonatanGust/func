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
o = emptyO
p = Black

decidePlayer :: Brick -> IO Brick
decidePlayer Black = if canPM o White
                        then return White
                        else return Black
decidePlayer White = if canPM o Black
                        then return Black
                        else return White
main = do
          canvas <- (mkCanvas "white")
          column documentBody [canvas]
          Just can <- fromElem (canvas)
          render can (drawSquare 0 0 "black" )
          renderOnTop can (drawSquare 100 100 "green" )
          drawAll can o
          --mapM (renderOnTop can) $ renderAll o
          canvas `onEvent` Click $ \mouse -> do
                         let (x,y)  = mouseCoords mouse
                             pos    = (fromIntegral (x), fromIntegral (y))
                             cX = x `div` brickSizeInt--(toInteger brickSize)
                             cY = y `div` brickSizeInt--(toInteger brickSize)
                             (b,on) = tryPB o (cX,cY) Black
--                         renderOnTop can (drawSquare 100 100 "white" )
--                         renderOnTop can (drawSquare 200 200 "black" )
--                         drawAll can o
                         --mapM_ (renderOnTop can) $ renderAll on


                         if b
                            then do o <- return on
                                    p <- decidePlayer p
                                    mapM_ (renderOnTop can) $ renderAll on
                            else return ()




--                         renderOnTop can (drawSquare (fst pos) (snd pos) "white" )


            where
          renderAll o = map (renderSquare) $ (concat (rows o)) `zip` posList
          drawAll can o = mapM (renderOnTop can) $ renderAll o

renderSquare :: (Maybe Brick, (Pos)) -> Picture ()
renderSquare (b,(x,y)) | b == (Just Black) = (drawSquare (doublex*brickSize) (doubley*brickSize) "black" )
                       | b == (Just White) = (drawSquare (doublex*brickSize) (doubley*brickSize) "white" )
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
