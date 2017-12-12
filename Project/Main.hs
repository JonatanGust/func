module Main where

import Othello
import Haste.Graphics.Canvas
import Haste.DOM
import Haste.Events
import Haste.Prim
import Haste.Foreign
import Data.IORef

brickSize = 90
fieldSize = brickSize * 8
--The main function does the following:
--Creates canvas for both game board, text fields and a button
--Adds them to a column div in the dokument body
--ot is our state, containging the game board and active player
--      is initiated to the starting gameboard and with player black as first
--We draw the default game board
--We draw text in the bottom canvas
--Now we add a click handler for the canvas,
--      here we check where the click happened and try to do a move,
--      using tryPB from the Othello module, if the move was legal we update
--      our state and also check if there is more than 1 lega lmove for the
--      next player, if there isn't we do that move for the player (recursive)
--      After all that we repaint the game board and text, and lastly we do
--      a check to see if the game has ended, if so we print the winner in the
--      text canvas.
--Now we add a click handler to the button (restart) that clears the current
--      body and then reinitiates the main function.
--In the where section we have
--  1)  renderAll wich creates pictures() (from haste) of the whole gameboard
--  2)  drawAll wich draws all the renders
main = do
      canvas <- (mkCanvas (fieldSize,fieldSize) "white")
      txtCanvas <- (mkCanvas (fieldSize/8,fieldSize) "white")
      restartBtn <- (mkButton "Restart")
      column documentBody [canvas,txtCanvas,restartBtn]
      Just can <- fromElem (canvas)
      ot <- newIORef (emptyO, Black)
      drawAll can emptyO
      Just txtCan <- fromElem (txtCanvas)
      renderText txtCan (emptyO,Black)
      canvas `onEvent` Click $ \mouse -> do
                     (oc,p) <- readIORef ot
                     let (x,y)  = mouseCoords mouse
                         cX = x `div` (round brickSize)
                         cY = y `div` (round brickSize)
                         (b,no) = tryPB oc (cX,cY) p
                         np     = decidePlayer no p
                     if b
                        then
                            do --let (lo, lp) = helpPlayers on np
                               writeIORef ot (no,np)
                               mapM_ (renderOnTop can) $ renderAll no
                               renderText txtCan (no,np)
                        else return ()
                     --(oc,p) <- readIORef ot
                     if null $ allLM no
                        then renderWinner no txtCan (getWinner no)
                        else return ()

      restartBtn `onEvent` Click $ \_ -> do
           clearChildren documentBody
           main
           return ()

      where
        renderAll o = map (squarePicture) $ (concat (rows o)) `zip` posList
        drawAll can o = mapM (renderOnTop can) $ renderAll o

-- displays the scores and the winner as the player with the most points
renderWinner :: Othello -> Canvas -> Maybe Brick -> IO ()
renderWinner o txtCan b = do
       render txtCan ( scale (2,2)   (text (20,20) ("White score: "++(show (getPS o White)))))
       renderOnTop txtCan ( scale (2,2)   (text (120,20) ("Black score: "++ (show (getPS o Black)))))
       renderOnTop txtCan ( scale (2,2)   (text (220,20) (winner)))
       where winner = if b == Nothing then "It's a draw!"
                        else if b == (Just Black) then "Black won!"
                                else "White won!"

--displays the scores and who's turn it is
renderText :: Canvas -> (Othello,Brick) -> IO ()
renderText txtCan (o,b) = do
    render txtCan ( scale (2,2)   (text (20,20) ("White score: "++(show (getPS o White)))))
    renderOnTop txtCan ( scale (2,2)   (text (120,20) ("Black score: "++ (show (getPS o Black)))))
    renderOnTop txtCan ( scale (2,2)   (text (220,20) ("It's "++(show b)++" player's turn")))

--takes a brick and position and returns the equivalent picture to be drawn
squarePicture :: (Maybe Brick, (Pos)) -> Picture ()
squarePicture (b,(x,y)) | b == (Just Black) =
                (drawCircle (doublex*brickSize) (doubley*brickSize) "black" )
                       | b == (Just White) =
                (drawCircle (doublex*brickSize) (doubley*brickSize) "white" )
                       | otherwise         =
                (drawSquare (doublex*brickSize) (doubley*brickSize) "green" )
                       where
                           doublex = fromIntegral x
                           doubley = fromIntegral y

--a circleShape with diameter a little smaller than brickSize
circleShape :: Double -> Double -> Shape ()
circleShape x y = circle (x+(brickSize/2),y+(brickSize/2))
    ((brickSize - (brickSize / 10)) /2)

--a squareShape with width and height brickSize
squareShape :: Double -> Double -> Shape ()
squareShape x y = rect ((x, y)) (((x+brickSize), (y+brickSize)))

--draws a circleShape with border
drawCircle :: Double -> Double -> String -> Picture ()
drawCircle x y clr = do
     color (getRGB "green") $ fill $ squareShape x y
     stroke $ squareShape x y
     color (getRGB clr) $ fill $ circleShape x y
     stroke $ circleShape x y

--draws a squareShape with border
drawSquare :: Double -> Double -> String -> Picture ()
drawSquare x y clr = do
    color (getRGB clr) $ fill $ squareShape x y
    stroke $ squareShape x y

--gets RGB from a color string
getRGB clr | clr == "green" = (RGB 0   255 0)
           | clr == "black" = (RGB 0   0   0)
           | clr == "white" = (RGB 255 255 255)


----------Methods copied from Pages.hs---------------------------------------
---http://www.cse.chalmers.se/edu/year/2016/course/TDA555/Haste/Pages.hs-----
-----------------------------------------------------------------------------

-- `mkCanvas width height` makes a drawing canvas of the specified dimensions
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
