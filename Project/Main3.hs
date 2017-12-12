module Main3 where

import WebFudgets
import HasteExtras(addStyleLink)
import Haste.Graphics.Canvas
import Haste.DOM
import Haste.Events
import Data.IORef
import Othello

main = do addStyleLink "demoo.css"
          runF (h2F (textF "Othello") >+ listFud )

example1 = emptyO

boxTable = (tableF 8 (canvasList example1)) `withF` css
                where css = [style "border-collapse" =: "collapse",
                             style "border-spacing" =: "0px 0",
                             style "line-height" =: "0px"]

canvasList :: Othello -> (F (Picture ()) (MouseEvent, MouseData))
canvasList (Othello [])                     = emptybox
canvasList (Othello [[]])                   = emptybox
canvasList (Othello ([]:xs))                = canvasList (Othello xs)
canvasList (Othello o) | elem == Just Black = blackbox >+ (canvasList newO)
                       | elem == Just White = whitebox >+ (canvasList newO)
                       | otherwise          = greenbox >+ (canvasList newO)
    where elem = head (head o)
          newO = Othello $ (drop 1 (head o)) : (drop 1 o)

--how to add/use eventhandlers?
canvasList' :: Othello -> [(F (Picture ()) (MouseEvent, MouseData))]
canvasList' (Othello [])                     = [emptybox]
canvasList' (Othello [[]])                   = [emptybox]
canvasList' (Othello ([]:xs))                = [canvasList (Othello xs)]
canvasList' (Othello o) | elem == Just Black = blackbox : [(canvasList newO)]
                       | elem == Just White = whitebox : [(canvasList newO)]
                       | otherwise          = greenbox : [(canvasList newO)]
    where elem = head (head o)
          newO = Othello $ (drop 1 (head o)) : (drop 1 o)



emptybox = canvasF (0,0)

boxW = 40

blackbox = box "black"
greenbox = box "green"
whitebox = box "white"

box :: String -> (F (Picture ()) (MouseEvent, MouseData))
box s = canvasF (boxW,boxW) `withF` css
            where css = [style "border" =: "1px solid black",
                   style "background" =: s]

--getImage :: (F (Picture ()) (MouseEvent, MouseData)) -> (Picture ())
--getImage (F p _) = p


getImage' :: (F hi ho) -> hi
getImage' (F p _) = p



--getImage' :: (a b c) -> b
--getImage' ((a b c)) = b

--boxCord :: Pos -> String ->

--listFudget :: F (Picture ()) (MouseEvent, MouseData)
--listFudget = listF (zip posList (canvasList' example1))

listFud = listF [((1,1),blackbox)]


--listF
--picture as input
--loopF stateF
--return pos, hi