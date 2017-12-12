import Haste
import Haste.Graphics.Canvas
import Haste.DOM
import Haste.Events

import Data.IORef

import Pages



--------------------------------------------------------------------------------
-- Pure code, the program logic
-- We follow the philosophy of doing as much as possible in the pure code.

type Size = (Double,Double)

radius :: Double
radius = 15

type Ball  = [Point]
type State = [Ball]

bounce :: Size -> Point -> Int -> Ball
bounce (w,h) (x,y) v
   | v == 0 && y >= maxY = replicate 20 (x,y)
   | y' > maxY           = bounce (w,h) (x,y) (2-v)
   | otherwise           = (x,y) : bounce (w,h) (x,y') v'
 where
   maxY = h-radius
   v'   = v + 1
   y'   = y + fromIntegral v

-- We use Int to represent velocity, because we need to compare it to 0, which
-- is generally a bad idea for floating-point numbers (due to rounding errors).

step :: State -> State
step bs = [ ps | _:ps <- bs ]
  -- Two purposes:
  --
  --  * Drop the first point in each ball
  --  * Filter out finished balls (empty lists)



--------------------------------------------------------------------------------
-- Interactive code
-- This part is only concerned with interaction and drawing, not with the logic
-- determining the behavior of the balls.

ballShape :: Ball -> Shape ()
ballShape []      = return ()
ballShape (pos:_) = circle pos radius

drawBall :: Ball -> Picture ()
drawBall ball = do
    color (RGB 255 0 0) $ fill $ ballShape ball
    stroke $ ballShape ball

animate :: Canvas -> IORef State -> IO ()
animate can state = do
    balls <- readIORef state
    writeIORef state $ step balls
    render can $ mapM_ drawBall balls
    setTimer (Once 20) $ animate can state
    return ()

canWidth, canHeight :: Num a => a
canWidth  = 500
canHeight = 500

main = do
    canvas <- mkCanvas canWidth canHeight
    clear  <- mkButton "clear"
    column documentBody [canvas,clear]

    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"

    Just can <- getCanvas canvas

    -- Use an IORef to communicate between the animation and the event handlers
    state <- newIORef []

    -- Start the animation
    animate can state

    -- Set an event handler for clicks in the canvas
    canvas `onEvent` Click $ \mouse -> do
      let (x,y) = mouseCoords mouse
          pos   = (fromIntegral x, fromIntegral y)
          ball  = bounce (canWidth,canHeight) pos 0
      balls <- readIORef state
      writeIORef state (ball:balls)

    -- Set an event handler for the clear button
    clear `onEvent` Click $ \_ -> writeIORef state []

-- Note: The current version of Haste does not run the event handler
-- concurrently with the animation, so there's no risk of a race between the
-- different uses of `writeIORef`. If it was truly concurrent, then atomic
-- operations would have to be used instead (see the Data.IORef documentation).
