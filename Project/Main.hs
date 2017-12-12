module Main where

import Haste
import Haste.Graphics.Canvas
import Othello

import Haste.DOM
import Haste.Events


import Haste.Prim
import Haste.Foreign
import Data.IORef

--import Pages


--renderAll :: Othello -> IO ()
brickSize = 90

brickSizeInt :: Int
brickSizeInt = 90

size = brickSize * 8


main = do
          canvas <- (mkCanvas (size,size) "white")
          txtCanvas <- (mkCanvas (size/8,size) "white")
          restart <- (mkButton "Restart")
          column documentBody [canvas,txtCanvas,restart]
          Just can <- fromElem (canvas)
          ot <- newIORef (emptyO, Black)
          drawAll can emptyO
          Just txtCan <- fromElem (txtCanvas)
          renderText txtCan (emptyO,Black)
          canvas `onEvent` Click $ \mouse -> do
                         (oc,p) <- readIORef ot
                         let (x,y)  = mouseCoords mouse
--                             pos    = (fromIntegral (x), fromIntegral (y))
                             cX = x `div` brickSizeInt
                             cY = y `div` brickSizeInt
                             (b,on) = tryPB oc (cX,cY) p
                             np     = decidePlayer on p
                         if b
                            then
                                do writeIORef ot (on,np)
                                   mapM_ (renderOnTop can) $ renderAll on
                                   renderText txtCan (on,np)
                            else return ()
                         if null $ allLM on
                            then renderWinner on txtCan (getWinner on)
                            else return ()


          restart `onEvent` Click $ \_ -> do
           clearChildren documentBody
           main
           return ()

          where
            renderAll o = map (renderSquare) $ (concat (rows o)) `zip` posList
            drawAll can o = mapM (renderOnTop can) $ renderAll o

renderWinner :: Othello -> Canvas -> Maybe Brick -> IO ()
renderWinner o txtCan b = do
       render txtCan ( scale (2,2)   (text (20,20) ("White score: "++(show (getPS o White)))))
       renderOnTop txtCan ( scale (2,2)   (text (120,20) ("Black score: "++ (show (getPS o Black)))))
       renderOnTop txtCan ( scale (2,2)   (text (220,20) (winner)))
       where winner = if b == Nothing then "It's a draw!"
                        else if b == (Just Black) then "Black won!"
                                else "White won!"

renderText :: Canvas -> (Othello,Brick) -> IO ()
renderText txtCan (o,b) = do
    render txtCan ( scale (2,2)   (text (20,20) ("White score: "++(show (getPS o White)))))
    renderOnTop txtCan ( scale (2,2)   (text (120,20) ("Black score: "++ (show (getPS o Black)))))
    renderOnTop txtCan ( scale (2,2)   (text (220,20) ("It's "++(show b)++" player's turn")))


renderSquare :: (Maybe Brick, (Pos)) -> Picture ()
renderSquare (b,(x,y)) | b == (Just Black) =
                (drawCircle (doublex*brickSize) (doubley*brickSize) "black" )
                       | b == (Just White) =
                (drawCircle (doublex*brickSize) (doubley*brickSize) "white" )
                       | otherwise         =
                (drawSquare (doublex*brickSize) (doubley*brickSize) "green" )
                       where
                           doublex = fromIntegral x
                           doubley = fromIntegral y

decidePlayer :: Othello -> Brick -> Brick
decidePlayer o Black = if canPM o White
                        then White
                        else Black
decidePlayer o White = if canPM o Black
                        then Black
                        else White

squareShape :: Double -> Double -> Shape ()
squareShape x y = rect ((x, y)) (((x+brickSize), (y+brickSize)))

boardShape :: Double -> Double -> Shape ()
boardShape x y = rect ((x, y)) (x+(brickSize*8), (y+(brickSize*8)))

circleShape :: Double -> Double -> Shape ()
circleShape x y = circle (x+(brickSize/2),y+(brickSize/2))
    ((brickSize - (brickSize / 10)) /2)

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



----------Methods stolen from Pages.hs---------------------------------------

mkCanvas :: (Double,Double) -> String -> IO Elem
mkCanvas (h,w) s =
    newElem "canvas" `with` [style "border"          =: "1px solid black",
                             style "backgroundColor" =: s,
                             prop "width"            =: (show $w),
                             prop "height"           =: (show $h)]

-- `mkButton label` makes a clickable button with the given label
mkButton :: String -> IO Elem
mkButton label = newElem "button" `with` [prop "textContent" =: label]

-- column parent children adds the children as a column column to the parent
column :: Elem -> [Elem] -> IO ()
column parent children = do
    cs <- sequence [wrapDiv c | c <- children]
    appendChildren parent cs

-- appendChildren parent children adds a list of children to a parent element
appendChildren :: Elem -> [Elem] -> IO ()
appendChildren parent children =
    sequence_ [appendChild parent c | c <- children]

-- `wrapDiv e` makes a "div" node with `e` as the only child
wrapDiv :: Elem -> IO Elem
wrapDiv e = mkDiv `with` [children [e]]

-- `mkDiv` makes a container element for grouping elements together
mkDiv :: IO Elem
mkDiv = newElem "div"
