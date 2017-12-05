-- |  Web Fudgets Intro
-- Examples to introduce Web Fudgets (for declarative web programming)
-- Functional Programming course 2017.
-- Thomas Hallgren

{-
This started as a skeleton, the definitions were filled in during the lecture.
-}
--------------------------------------------------------------------------------

import WebFudgets
import HasteExtras(addStyleLink)
import Haste.Graphics.Canvas

main = do addStyleLink "demo.css"
          runF (h2F (textF "WebFudgets Intro") >+ examples)

examples = ex1 >+ ex2 >+ ex3 >+ ex4 >+ex5 >+ ex6 >+ ex7 >+ ex8 >+ex9 >+ex10

--------------------------------------------------------------------------------
-- * Hello world example

ex1 = shellF "Hello" $ textF "Hello world!"


--------------------------------------------------------------------------------
-- * Illustrates parallel composition

ex2 = shellF "Hello world with a button" $
      textF "Hello" >+< buttonF "Click me"


--------------------------------------------------------------------------------
-- * Illustrates serial composition and stateless application code

ex3 = shellF "A program to test the factorial function version 1" $
      numberF =>= mapF fac =>= showF
      
fac 0 = 1
fac n = n * fac (n-1)


--------------------------------------------------------------------------------
-- * Illustrates layout and decoration

ex4 = shellF "A program to test the factorial function version 2" $
      pF (textF "n=" +< smallF numberF)
      =>= mapF fac =>=
      pF (textF "n! = " +< showF)

smallF fudget = fudget `withF` [style "width" =: "5em"]

--------------------------------------------------------------------------------
-- * Illustrates stateful application code

ex5 = shellF "An Up Counter" $
      smallF showF =<= stateF incr 0 =<= buttonF "Up"
      
incr n BtnClick = (n+1,[n+1])

--------------------------------------------------------------------------------
-- * Illustrates how to handle output from a parallel composition

ex6 = shellF "An Up/Down Counter" $
      smallF showF =<= stateF count 0 =<= (buttonF "Up" >+< buttonF "Down")
  where
    count n (Left BtnClick) = (n+1,[n+1])
    count n (Right BtnClick) = (n-1,[n-1])

--------------------------------------------------------------------------------
-- * Illustrates parallel composition of a list of things

data CounterButton = Up | Down | Reset
                     deriving (Eq,Show,Bounded,Enum)

ex7 = shellF "An Up/Down/Reset Counter" $
      smallF showF =<= stateF count 0 =<=
      listF [(t,buttonF (show t)) | t <- [Up .. Reset]]
  where
    count n (Up,BtnClick) = (n+1,[n+1])
    count n (Down,BtnClick) = (n-1,[n-1])
    count n (Reset,BtnClick) = (0,[0])

--------------------------------------------------------------------------------
-- * Illustrates feedback loops

ex8 = shellF "A Loadable Up/Down Counter" $
      loopLeftF (mapF Left =<= smallF readShowF =<= stateF count 0)
      =<= (buttonF "Up" >+< buttonF "Down")
  where
    count n (Left new) = (new,[])
    count n (Right (Left BtnClick)) = (n+1,[n+1])
    count n (Right (Right BtnClick)) = (n-1,[n-1])


--------------------------------------------------------------------------------
-- * A larger example (more of the same)

data CalculatorButton
     = Plus | Minus | Times | Div
     | Enter | Clear | Digit Int
     deriving Eq


ex9 = shellF "A Simple Calculator" $
      showF =<= stateF calc [0] =<= keyboardF
  where
    keyboardF = tableF 4 buttonsF
    buttonsF = listF [d 7, d 8, d 9,op Div,
                      d 4, d 5, d 6,op Times,
                      d 1, d 2, d 3,op Minus,
                      clr,d 0, ent,op Plus]

    d n = (Digit n,buttonF (show n))
    op o = (o,buttonF (opLabel o))
    ent = op Enter
    clr = op Clear

    opLabel Plus  = "+"
    opLabel Minus = "-"
    opLabel Times = "×"
    opLabel Div   = "÷"
    opLabel Enter = "Ent"
    opLabel Clear = "C"

    calc (n:s)   (Digit d,_) = new (n*10+d) s
    calc s       (Enter,_)   = (0:s,[head s])
    calc s       (Clear,_)   = ([0],[0])
    calc (y:x:s) (Plus,_)    = new (x+y) s
    calc (y:x:s) (Minus,_)   = new (x-y) s
    calc (y:x:s) (Times,_)   = new (x*y) s
    calc (y:x:s) (Div,_)     = new (x `div` y) s
    calc s       _           = (s,[head s])

    new n s = (n:s,[n])

--------------------------------------------------------------------------------
-- * Illustrates the use of a canvas for graphical output

ex10 = shellF "Canvas example" $
       pF (textF "Draw a polygon with " +< readShowF >+ textF " sides.")
       =>= mapF polygon =>=
       canvasF (300,300)

polygon n = do color (RGB 255 255 128) (fill (path ps))
               color (RGB 0 0 255)     (stroke (path ps))
               text (20,20) "A simple canvas example"
  where
    ps = map corner [0..n]

    c@(cx,cy) = (150,150) -- center
    r = 100 -- radius of enclosing circle
    
    corner i = (cx+r*cos a,cy+r*sin a)
      where
        r = 100
        a = 2*pi*(real i/ real n)

real x = fromIntegral x