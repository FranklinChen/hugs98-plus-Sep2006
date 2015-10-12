module Main where

import Graphics.HGL

import Data.Array
import Control.Monad( when )
import Data.IORef

----------------------------------------------------------------

-- withColor  :: Color -> Graphic -> Graphic
-- withColor c = withRGB (colorTable ! c)

----------------------------------------------------------------

-- abbreviation for runGraphics - almost all tests that you run
-- will be of the form "rg w<n>" for some small positive n
rg = runGraphics

main = runGraphics $ parMany [w1,w2,w3,w4,w5,{-w6 p1,-}w7,w8,w9,w10,w13]

w0 = do
	w <- openWindow "My First Graphics Program" (400,400)
	wGetChar w
	closeWindow w

w1 = do
	w <- openWindow "My First Graphics Program" (400,400)
	drawInWindow w $ text (150,200) "Hello Graphics Window 1"
	getLBP w
	drawInWindow w $ text (150,250) "Hit a key" 
	wGetChar w
	clearWindow w 
	drawInWindow w $ text (150,200) "Thank you"
        drawInWindow w $ text (150,250) "Hit another key"
	wGetChar w
	closeWindow w

w2 = do
	w <- openWindow "My Second Graphics Program" (400,400)
	drawInWindow w $ text (150,200) "Hello Graphics Window 2"
	getRBP w
	drawInWindow w $ text (150,250) "Hit a key" 
	wGetChar w
	clearWindow w 
	drawInWindow w $ text (150,200) "Thank you"
        drawInWindow w $ text (150,250) "Hit another key"
	wGetChar w
	closeWindow w

----------------------------------------------------------------
-- Region demo
----------------------------------------------------------------

-- first create some regions

r1,r2,r3,r4 :: MyRegion
r1 = unions
	[ Shape (Rect    (000,000) (300,100))
	, Shape (Ellipse (100,000) (200,500))
	]
r2 = Shape (Rect (050,050) (250,150))
r3 = r1 `Intersect` r2
r4 = pentangle 0

pentangle :: Angle -> MyRegion
pentangle phi = Shape (poly phi (4*pi/5) 5)

triangle :: Angle -> MyRegion
triangle phi = Shape (poly phi (2*pi/3) 3)

-- draw a regular polygon with "n" sides "angle" apart 
poly :: Angle -> Angle -> Int -> Shape
poly init delta n = Polygon vertices
 where
  thetas   = take n [init, init+delta ..]
  vertices = [ (round x, round y)
             | theta <- thetas 
             , let x = 200*(1+cos theta)
	     , let y = 200*(1+sin theta)
             ]

----------------------------------------------------------------

w3 = do
	w <- openWindow "Regions demo" (300,500)
	mapM_ (\ r -> do
                        setGraphic w (drawRegion r)
                  	getLBP w)
              [r1,r2,r3,r4]
	clearWindow w
	drawInWindow w $ text (150,250) "Hit a key" 
	wGetChar w
	closeWindow w

w4 = do 
	wa <- openWindow "Multiwindow demo 1" (300,500)
	wb <- openWindow "Multiwindow demo 2" (300,500)

        drawInWindow wa $ text (100,100) "Hello World 1"
        drawInWindow wb $ text (100,100) "Hello World 2"

	ka <- wGetChar wa
        drawInWindow wb $ text (100,200) ("Got key " ++ [ka])

	kb <- wGetChar wb
        drawInWindow wa $ text (100,200) ("Got key " ++ [kb])

	closeWindow wa
	closeWindow wb

----------------------------------------------------------------
-- Picture demo
----------------------------------------------------------------

type Pic color = [(color, MyRegion)] 

p1 = [ (Red,r3), (Green, r2), (Blue, r1)]

w5 = do
	w <- openWindow "Picture demo" (300,500)
	setGraphic w (drawPic p1)
        getLBP w
	clearWindow w
	drawInWindow w $ text (150,250) "Hit a key" 
	wGetChar w
	closeWindow w

drawPic :: Pic Color -> Graphic
drawPic p = overGraphics [ withColor c $ drawRegion r | (c,r) <- p ]

----------------------------------------------------------------
-- Faster picture drawing
----------------------------------------------------------------

-- This version is more efficient because it allocates colors just once.
--
-- The cost of this performance gain is that we have to expose the
-- allocation and deallocation of brushes to the programmer, which 
-- makes it possible for them to deallocate too early, to deallocate
-- too late (eg never), or to deallocate too often.
--
-- Notice that you must not deallocate brushes (and fonts, etc)
-- until _after_ you clear the screen - otherwise the redraw routine
-- might be called and you'll have a dangling reference to the brush.

-- w6 p = do
--         w <- openWindow "Faster Picture Demo" (300,500)
-- 
--         -- get the "palette"
--         let colors  =  nub [ c | (c,r) <- p ]
--         brushes     <- mapM (\c -> mkBrush (colorTable ! c)) colors
--         let palette =  array (minBound,maxBound) (zip colors brushes)
--         let pic     =  [ (palette!c, r) | (c,r) <- p ]
-- 
--         setGraphic w (drawPic2 pic)
--         getLBP w
-- 
--         clearWindow w
-- 
--         drawInWindow w $ text (150,250) "Hit a key" 
--         wGetChar w
--         closeWindow w

drawPic2 :: Pic Brush -> Graphic
drawPic2 p = overGraphics [ withBrush b (drawRegion r) | (b,r) <- p ]

----------------------------------------------------------------
-- Animation (sort of)
--
-- Simple animations made of lists of pictures
----------------------------------------------------------------

type Frame = Pic Color
type Anim  = [Frame]

a1 :: Anim
a1 = [ [(Red,   pentangle phi)
       ,(Green, triangle  (-phi))
       ]
     | phi <- [0, pi/20 .. 2*pi] 
     ]

-- draw an animation (using user-input to step through animation)
-- Note that this doesn't terminate until the window is closed
-- which has to be done using your window manager.
w7 = do
	w <- openWindow "Animation demo" (400,400)
	mapM_ (drawFrame w) a1
 where
  -- draw a frame and wait for left button press
  drawFrame :: Window -> Frame -> IO ()
  drawFrame w p = do
	  setGraphic w (drawPic p)
	  getLBP w
	  return ()

----------------------------------------------------------------
-- Timer demo
----------------------------------------------------------------

-- draw an animation (using timer to step through animation)
w8 = do
	w <- openWindowEx "Timer demo" Nothing (400,400)
                          DoubleBuffered (Just 50)
	mapM_ (drawFrame w) (cycle a1)
 where
  -- draw a frame and wait for a tick
  drawFrame :: Window -> Frame -> IO ()
  drawFrame w p = do
	setGraphic w (drawPic p)
	getWindowTick w

----------------------------------------------------------------
-- Text demo
----------------------------------------------------------------

-- half-inch wide, red text on a transparent background at a 45 degree angle
-- quarter-inch wide, red text on a green background at a -45 degree angle
w9 = do
	w <- openWindow "Font demo" (500,500)
        font1 <- createFont (50,50)  (pi/4)  False False "helvetica"
        font2 <- createFont (25,50) (-pi/4) True True   "times"
        drawInWindow w
          $ withTextColor (RGB 255 0 0)
          $ withFont font1
          $ withBkMode Transparent
  	  $ text (050,450) "Font Test 1"
        drawInWindow w
          $ withTextColor (RGB 0 0 255)
          $ withFont font2 
          $ withBkMode Opaque
          $ withBkColor (RGB 0 255 0)
          $ text (050,050) "Font Test 2"
	getLBP w
        deleteFont font1
        deleteFont font2
	closeWindow w

----------------------------------------------------------------
-- Error catching demo
----------------------------------------------------------------

-- This program demonstrates that the system doesn't get left in
-- an inconsistent state even if your program hits an error.

w10 = do
	w <- openWindow "Error recovery demo" (300,300)
        drawInWindow w
  	  $ text (10,150) "Click me to test error recovery"
	getLBP w
        drawInWindow w $ error "foo1"
	--error "foo2"
	getLBP w
	clearWindow w
        drawInWindow w
  	  $ text (10,150) "Shouldn't have made it this far"
	getLBP w
	closeWindow w

----------------------------------------------------------------
-- Bitmap demo
----------------------------------------------------------------

{-
w11 = do
	w <- openWindow "Bitmap demo" (300,500)
	setGraphic w $ 
          text (150,200) "Test"
          `overGraphic`
          drawPic p1
	Draw.saveGraphic "bitmaps/Foo.bmp" (300,500) $ 
          text (150,200) "Test"
          `overGraphic`
          drawPic p1
        getLBP w
	closeWindow w

w11b = do
	(bmp,_) <- Bitmap.load "bitmaps/Foo.bmp" 
	w <- openWindow "Bitmap demo" (300,500)
	setGraphic w $ Bitmap.draw (50,50) bmp
	wGetChar w
	closeWindow w

w11c = do
	(bmp,_) <- Bitmap.load "bitmaps/Foo.bmp" 
	w <- openWindow "Bitmap demo" (300,500)
	setGraphic w $ Bitmap.drawStretched (0,400) (400,0) bmp
	wGetChar w
	closeWindow w

w12 = do
	w <- openWindow "Bitmap demo" (400,400)
	mapM_ (drawFrame w) (zip [100..] a1)
 where
  -- draw a frame and wait for left button press
  drawFrame :: Window -> (Int,Frame) -> IO ()
  drawFrame w (i,p) = do
	  setGraphic w $ drawPic p
 	  Draw.saveGraphic name (400,400) $ drawPic p
	  getLBP w
	  return ()
   where
    name = "bitmaps/" ++ "Foo" ++ show i ++ ".bmp"
-}

w13 = do
	w <- openWindow "My First Incremental Graphics Program" (400,400)
        -- start with 2 elements in the list so that polyline doesn't crash
	pointRef <- newIORef [(0,0),(400,400)]
	let
	  -- discrete lines between left clicks, right click to move on
	  loop1 = do
		e <- getWindowEvent w
		case e of
		 Button{pt=pt,isDown=isDown,isLeft=isLeft}
		   | not isLeft -> return ()
                   | isDown -> addPoint pt >> loop1
		 Closed -> return ()
		 _ -> loop1

	  -- continuous lines as long as mouse is held down
	  loop2 down = do
		e <- getWindowEvent w
		case e of
		 Button{isDown=isDown}-> loop2 isDown
		 MouseMove{pt=pt} -> do
		         when down (addPoint pt)
		         loop2 down
		 Closed -> return ()
		 _ -> loop2 down

	  -- code to do a total redraw
	  drawPoints = do
		pts <- ioToDraw $ readIORef pointRef
		withColor Red $ polyline pts

	  -- code to draw the bit that changed and record the change
	  addPoint pt = do
		pts <- readIORef pointRef
		writeIORef pointRef (pt:pts)
		directDraw w $ withColor Red $ line pt (head pts)
		--uncomment the next line to see how bad a total redraw would be
--		redrawWindow w

	setGraphic w drawPoints
	loop1
	loop2 True
	closeWindow w

----------------------------------------------------------------
-- Examples of straight pictures (not regions)
----------------------------------------------------------------

shapeToGraphic :: Shape -> Graphic
shapeToGraphic shape =
  case shape of
   Rect (x1,y1) (x2,y2) -> polygon [(x1,y1),(x2,y1),(x2,y2),(x1,y2)]
   Ellipse p1 p2        -> ellipse p1 p2
   Polygon pts          -> polygon pts

s1 = Rect (100,100) (200,300)
s2 = Ellipse (150,150) (300,200)
s3 = poly 0 (4*pi/5) 5

w14 = do
	w <- openWindow "Pure Picture Demo" (400,400)
        drawInWindow w $ withColor Red    $ shapeToGraphic s1
        drawInWindow w $ withColor Blue   $ shapeToGraphic s2
        drawInWindow w $ withColor Yellow $ shapeToGraphic s3
        suspend w

suspend = loop
  where loop w = getWindowEvent w >> loop w

pic1 = withColor Red  $ 
         ellipse (150,150) (300,200)
pic2 = withColor Blue $ 
         polyline [(100,50),(200,50),(200,250),(100,250),(100,50)]

testGraphics = runGraphics $
  do w <- openWindow "Some Graphics Figures" (300,300)
     drawInWindow w pic1
     drawInWindow w pic2
     suspend w

----------------------------------------------------------------
-- Examples from the documentation
----------------------------------------------------------------

demos :: IO ()
demos = runGraphics $ parMany
  [ helloWorld
  , eg cp
--  , fontDemo
  , lineDemo 0
  , lineDemo 1
  , lineDemo 20
  , keyTest
  , timerDemo
  , ellipseTest
  ]

helloWorld :: IO ()
helloWorld = do
	  w <- openWindow "Hello World Window" (300, 300)
	  drawInWindow w $ text (100, 100) "Hello"
	  drawInWindow w $ text (100, 200) "World"
	  wGetChar w
	  closeWindow w

cp :: Graphic
cp = 
     mkBrush (colorTable!Red)  $ \ red  ->
     mkBrush (colorTable!Blue) $ \ blue ->
     overGraphic
       (withBrush red  $ polygon [(200,200),(400,200),(300,400)])
       (withBrush blue $ polygon [(100,100),(500,100),(500,500),(100,500)])

eg :: Graphic -> IO ()
eg p = do
	  w <- openWindow "Hello World Window" (600,600)
	  drawInWindow w p
	  wGetChar w
	  closeWindow w

-- fontDemo :: IO ()
-- fontDemo = do
--   w <- openWindow "Font Demo Window" (500,500)
--   drawInWindow w $
--     withTextColor (RGB 255 0 0)           $
--     mkFont (50,100) (pi/4) False True "Arial"  $ \ font ->
--     withFont font		          $
--     withBkColor (RGB 0 255 0)             $
--     withBkMode Opaque         	          $
--     text (050,450) "Font Demo"
--   wGetChar w
--   closeWindow w  

-- Note that "width" must be 1 or less for the penstyle to matter

lineDemo :: Int -> IO ()
lineDemo width = do
  w <- openWindow ("Line Demo Window " ++ show width) (500,500)
  drawInWindow w $
    let 
      color = colorTable ! Red 
    in
    mkPen Solid      width color $ \ pen1 ->
    mkPen Dash       width color $ \ pen2 ->
    mkPen Dot        width color $ \ pen3 ->
    mkPen DashDotDot width color $ \ pen4 ->
    overGraphics 
      [ withPen pen1 $ line (100,100) (400,100)
      , withPen pen2 $ line (100,200) (400,200)
      , withPen pen3 $ line (100,300) (400,300)
      , withPen pen4 $ line (100,400) (400,400)
      ]
  wGetChar w
  closeWindow w

-- Just what keys can we see?
keyTest :: IO ()
keyTest= do
  w <- openWindow "Keypress Demo Window" (500,500)
  c <- wGetChar w
  print (fromEnum c)
  closeWindow w


-- Tick counter
timerDemo = do
  w <- openWindowEx 
         "Timer demo"         -- title
         (Just (500,500))     -- initial position of window
         (100,100)            -- initial size of window
         drawFun              -- draw function - see below
         (Just 50)            -- tick rate
  let
    loop x = do
      setGraphic w $ text (0,50) $ show x
      getWindowTick w          -- wait for next tick on window
      loop (x+1)
  loop 0
 where

  -- The possible choices of "drawFun" are
  --
  -- o drawBufferedGraphic - use a double buffer to reduce animation flicker
  -- o drawGraphic         - draw directly to screen (for speed)
  
  useDoubleBuffering = True

  drawFun = if useDoubleBuffering then
               DoubleBuffered
            else
               Unbuffered

ellipseTest :: IO ()
ellipseTest = do
	  w <- openWindow "Ellipse Test" (300, 300)
	  drawInWindow w $ ellipse (0,0) (200, 100)
	  wGetChar w
	  closeWindow w


----------------------------------------------------------------
-- Region code - prototype of the code in SOE
----------------------------------------------------------------

{-
module Region(
	MyRegion(Empty, Union, Intersect, Shape),
	unions,
	Shape(Rect, Ellipse, Polygon),
	drawRegion
	) where

import GraphicsRegion
-}

data MyRegion
  = Empty
  | Union     MyRegion MyRegion
  | Intersect MyRegion MyRegion
  | Shape     Shape

unions :: [MyRegion] -> MyRegion
unions = foldr Union Empty 

--intersects :: [MyRegion] -> MyRegion
--intersects = foldr Intersect Full

drawRegion :: MyRegion -> Graphic
drawRegion r = regionToGraphic (regionToRGN r)

regionToRGN :: MyRegion -> Region

regionToRGN Empty           = rectangleRegion (0,0) (0,0)
regionToRGN (Shape s)       = shapeToRGN s
regionToRGN (r1 `Union` r2) = unionRegion (regionToRGN r1) (regionToRGN r2)
regionToRGN (r1 `Intersect` r2) = intersectRegion (regionToRGN r1) (regionToRGN r2)

data Shape 
  = Rect    Point Point
  | Ellipse Point Point
  | Polygon [Point]

shapeToRGN :: Shape -> Region
shapeToRGN (Rect    p1 p2) = rectangleRegion p1 p2
shapeToRGN (Ellipse p1 p2) = ellipseRegion   p1 p2
shapeToRGN (Polygon ps)    = polygonRegion   ps

----------------------------------------------------------------
-- End
----------------------------------------------------------------
