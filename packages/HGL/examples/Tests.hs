module Main( main ) where

import Graphics.HGL
import Control.Exception( try )

----------------------------------------------------------------
-- Main
----------------------------------------------------------------

main :: IO ()
main = rgs [ w1, w2 polys, w9, w10 ]

----------------------------------------------------------------
-- Other Examples
----------------------------------------------------------------

w1 = do
  w <- openWindow "Test" (400,400)
  setGraphic w (text (100,100) "Press a key")
  getKey w
  red   <- createPen Solid 4 (RGB 255 0 0)
  green <- createPen Solid 4 (RGB 0 255 0)
  blue  <- createPen Solid 4 (RGB 0 0 255)
  setGraphic w $ p1 
  getKey w
  setGraphic w $ (withPen red $ regular 3 (200,200) 100) `overGraphic` regular 4 (200,200) 100
  getKey w
  setGraphic w $ regular 4 (200,200) 100
  getKey w
  setGraphic w $ regular 5 (200,200) 100
  getKey w
  setGraphic w emptyGraphic
  getKey w
  closeWindow w
  return ()

w2 ps = do
  w <- openWindow "Animation" (400,400)
  setGraphic w (withTextColor (RGB 255 0 0) $ text (100,100) "Press a key")
  getKey w
  sequence_ [ setGraphic w p >> getKey w | p <- ps ]
  closeWindow w

polys = [ regular i (200,200) 100 | i <- [3..20] ]

p1 = overGraphics
    [ line (100,100) (200,100)
    , line (200,100) (200,200)
    , line (200,200) (100,200)
    , line (100,200) (100,100)
    ]

p2 = polyline
    [ (100,100)
    , (200,100)
    , (200,200)
    , (100,200)
    , (100,100)
    ]


regular :: Int -> Point -> Double -> Graphic
regular n = poly 0.0 (2*pi / fromIntegral n) n

-- draw a regular polygon with "n" sides "angle" apart 
poly :: Angle -> Angle -> Int -> Point -> Double -> Graphic
poly init delta n (x,y) radius = polyline vertices
 where
  thetas   = take (n+1) [init, init+delta ..] 
  vertices = [ (x + round dx, y + round dy)
             | theta <- thetas 
             , let dx = radius * cos theta
	     , let dy = radius * sin theta
             ]

rg = runGraphics
rgs = rg . parMany

-- overMany :: [Graphic] -> Graphic
-- overMany = foldr overGraphic emptyGraphic

w9 = do
	font1 <- createFont (50,50)  (pi/4) False False "courier"
	font2 <- createFont (25,50) (-pi/4) True True   "lucida"
	w <- openWindow "Font demo" (500,500)
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

w10 = do
	font1 <- createFont (10,20)  0 False False "helvetica"
	w <- openWindow "Font demo 2" (600,600)
        drawInWindow w $ withFont font1 $ overGraphics $
          [ test h v
          | h <- [ Left' .. Right' ]
          , v <- [ Top .. Bottom   ]
          ]
	getLBP w
	deleteFont font1
	closeWindow w
 where
  (x,y) = (300, 300)
  r     = 4
  test h v = withTextAlignment (h,v) $ text (x', y') (show (h,v))
             `overGraphic`
             ellipse (x'-r, y'-r) (x'+r, y'+r)
   where 
    dx = 250 * (fromEnum h - 1)
    dy = 250 * (fromEnum v - 1)
    x' = x + dx
    y' = y + dy  

w11a = w11 True
w11b = w11 False

w11 safe = do
	fonts' <- getFonts safe fonts
	w <- openWindow "Font demo" (600,600)
        drawInWindow w 
           $ overGraphics
           $ map (\(f,y) -> withFont f $ text (50,y) "Font Test")
           $ zip fonts' [0,50..]
	getLBP w
	closeWindow w
	mapM_ deleteFont fonts'
 where
  fonts = [ "courier"
          , "helvetica"
          , "times"
          , "lucida"
          , "new century schoolbook" 
          , "symbol"
          , "utopia"
          , "charter"
          , "gothic"
          , "terminal"
--          , "song"
          , "clean"
          , "fixed"
          ]

getFonts :: Bool -> [String] -> IO [Font]
getFonts safe fonts

  -- When used in safe mode, we protect against the distinct possibility
  -- that your machine doesn't have all the fonts you want.
  | safe 
  = do
      fonts' <- mapM (try . createFont (50,50) 0 False False) fonts
      return [ f | Right f <- fonts' ]

  -- When used in unsafe mode, you get whatever error message the underlying
  -- system feels like giving you.
  | otherwise 
  = do
      mapM (createFont (50,50) 0 False False) fonts

ellipseTest :: IO ()
ellipseTest = do
	  w <- openWindow "Ellipse Test" (300, 500)
	  drawInWindow w $ ellipse (0,0) (200, 100)
	  drawInWindow w $ shearEllipse (200, 100) (0,100) (200,200)
          let (x0,y0) = (100, 300)
              r       = 50
	  drawInWindow w $ overGraphics $
             [ shearEllipse
                 (x0,y0)
                 (x0 + round (r*cos a),  y0 + round (r*sin a))
                 (x0 + round (r*cos a'), y0 + round (r*sin a'))
             | a <- take 5 [ 0, 2*pi/5 .. ]
             , let a' = a + pi/4
             ]
	  drawInWindow w $ overGraphics $
             [ parallelogram
                 (x0,y0)
                 (x0 + round (r*cos a),  y0 + round (r*sin a))
                 (x0 + round (r*cos a'), y0 + round (r*sin a'))
             | a <- take 5 [ 0, 2*pi/5 .. ]
             , let a' = a + pi/4
             ]
	  getKey w
	  closeWindow w

parallelogram (x0,y0) (x1,y1) (x2,y2) = polyline pts
 where
  pts = [ (x0,y0), (x1,y1), (x1+x2-x0,y1+y2-y0), (x2,y2), (x0,y0) ]

----------------------------------------------------------------
-- End
----------------------------------------------------------------
