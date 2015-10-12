-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.X11.Window
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (requires concurrency)
--
-- A simple graphics library.
--
-----------------------------------------------------------------------------

-- #hide
module Graphics.HGL.X11.Window
	( runGraphicsEx		-- :: String -> IO () -> IO ()

	, Window(events, graphic)
	, openWindowEx		-- :: Title -> Maybe Point -> Size ->
                		--    RedrawMode -> Maybe Time -> IO Window
	, closeWindow		-- :: Window -> IO ()
	, getWindowRect		-- :: Window -> IO (Point,Point)
	, redrawWindow		-- :: Window -> IO ()
	, directDraw		-- :: Window -> Graphic -> IO ()

	, sendTicks, findWindow, showEvent
	) where

import Graphics.HGL.Internals.Types
import Graphics.HGL.Internals.Draw (Graphic, Draw, unDraw)
import Graphics.HGL.Internals.Event
import qualified Graphics.HGL.Internals.Utilities as Utils
import qualified Graphics.HGL.Internals.Events as E
import Graphics.HGL.X11.Types
import Graphics.HGL.X11.Display
import Graphics.HGL.X11.DC
import qualified Graphics.HGL.X11.Timer as T

import qualified Graphics.X11.Xlib as X

import Control.Concurrent	(forkIO, yield)
import Control.Concurrent.MVar	(MVar, newMVar, takeMVar, putMVar, readMVar)
import Control.Exception	(finally)
import Control.Monad		(when)
import Data.Bits
import Data.IORef		(IORef, newIORef, readIORef, writeIORef)
import Data.Maybe		(isJust, fromJust, fromMaybe)
import System.IO.Unsafe		(unsafePerformIO)

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

data Window = MkWindow  
  { wnd     :: X.Window	         -- the real window
  , ref_dc  :: MVar (Maybe DC)   -- "device context"
  , exposed :: IORef Bool        -- have we had an expose event yet?
  , events  :: E.Events          -- the event stream
  , graphic :: MVar Graphic      -- the current graphic
  , redraw  :: RedrawStuff
  , timer   :: Maybe T.Timer
  }

openWindowEx :: Title -> Maybe Point -> Size -> 
                RedrawMode -> Maybe Time -> IO Window

closeWindow    :: Window -> IO ()
getWindowRect  :: Window -> IO (Point,Point)

redrawWindow :: Window -> IO ()
directDraw :: Window -> Graphic -> IO ()

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

----------------------------------------------------------------
-- Windows
----------------------------------------------------------------

closeWindow' :: Bool -> Window -> IO ()
closeWindow' destroyXWindow w = do
  mb_dc <- takeMVar (ref_dc w)
  case mb_dc of
   Just dc -> do
     putMVar (ref_dc w) Nothing -- mark it for dead
     X.freeGC (disp dc) (textGC  dc)
     X.freeGC (disp dc) (paintGC dc)
     X.freeGC (disp dc) (brushGC dc)
     case (redraw w) of
         UnbufferedStuff -> return ()
         BufferedStuff gc _ ref_mbuffer -> do
             X.freeGC (disp dc) gc
             removeBuffer dc ref_mbuffer	    
     when destroyXWindow $ do
       X.destroyWindow (disp dc) (drawable dc)  -- ths dc had better hold a window!
       minor_eloop (disp dc)
   Nothing -> do
     putMVar (ref_dc w) Nothing

removeBuffer :: DC -> IORef (Maybe X.Pixmap) -> IO ()
removeBuffer dc ref_mbuffer = do
  mbuffer <- readIORef ref_mbuffer
  case mbuffer of
    Nothing -> return ()
    Just buffer -> X.freePixmap (disp dc) buffer
  writeIORef ref_mbuffer Nothing

removeDeadWindows :: IO ()
removeDeadWindows = do
  ws <- takeMVar wnds
  ws' <- remove ws []
  putMVar wnds ws'
 where
  remove [] r = return r
  remove (w:ws) r = do
    mb_dc <- readMVar (ref_dc w)
    if (isJust mb_dc) 
      then remove ws (w:r)
      else remove ws r

closeAllWindows :: IO ()
closeAllWindows = do
  ws <- readMVar wnds
  mapM_ (closeWindow' True) ws
  removeDeadWindows -- bring out your dead

sendTicks :: IO ()
sendTicks = do
  ws <- readMVar wnds
  sequence_ [ E.sendTick (events w) | w <- ws ]

-- persistent list of open windows
wnds :: MVar [Window]
wnds = unsafePerformIO (newMVar [])

-- persistent list of timers
timers :: T.Timers
timers = unsafePerformIO T.newTimers

runGraphicsEx :: String -> IO () -> IO ()
runGraphicsEx host m = do
  X.setDefaultErrorHandler
  display <- openDisplay host closeAllWindows
  T.clearTimers timers
--  color_map <- X.getStandardColormap display root X.a_RGB_BEST_MAP
  -- HN 2001-01-30
  -- There is a race condition here since the event loop terminates if it
  -- encounters an empty window list (in the global, imperative, variable
  -- wnds). Thus, if m has not yet opened a window (assuming it will!)
  -- when the event_loop is entered, it will exit immediately.
  -- Solution: wait until either the window list is non-empty, or until
  -- m exits (in case it does not open a window for some reason).
  mDone <- newIORef False
  forkIO (catchErrors m `finally` writeIORef mDone True)
  let loop = do
      yield
      ws <- readMVar wnds      
      d  <- readIORef mDone
      if not (null ws) then
          main_eloop display
       else if not d then
	  loop
       else
	  return ()
  catchErrors loop
--  X.sync display True
  closeAllWindows
--  X.sync display True
  -- A final yield to make sure there's no threads thinking of
  -- accessing the display
  yield
  closeDisplay

catchErrors :: IO () -> IO ()
catchErrors m = do
  r <- Utils.safeTry m
  case r of
    Left  e -> do
--      putStr "Uncaught Error: "
      print e  
    Right _ -> return ()
  return ()

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

openWindowEx name pos size redrawMode tickRate = do
  display <- getDisplay
  let
    corner@(X.Point x y) = fromPoint (fromMaybe (0,0) pos)
    (w,h) = fromSize  size
  let screen    = X.defaultScreenOfDisplay display
      fg_color  = X.whitePixelOfScreen screen
      bg_color  = X.blackPixelOfScreen screen
      depth     = X.defaultDepthOfScreen screen
      root      = X.rootWindowOfScreen screen
      visual    = X.defaultVisualOfScreen screen

  -- ToDo: resurrect the old code for constructing attribute sets
  window <- X.allocaSetWindowAttributes $ \ attributes -> do
    X.set_background_pixel attributes bg_color  
    let event_mask = 
     	  (   X.buttonPressMask
     	  .|. X.buttonReleaseMask
     	  .|. X.keyPressMask
     	  .|. X.keyReleaseMask
     	  .|. X.pointerMotionMask
     	  .|. X.exposureMask
     	  .|. X.structureNotifyMask
     	  )
    X.set_event_mask attributes event_mask
    -- We use backing store to reduce the number of expose events due to
    -- raising/lowering windows.
    X.set_backing_store attributes X.whenMapped
    -- We use bit-gravity to avoid generating exposure events when a window is
    -- made smaller (they can't be avoided when the window is enlarged).
    -- The choice of NW is somewhat arbitrary but hopefully works often
    -- enough to be worth it.
    X.set_bit_gravity attributes X.northWestGravity
    let attrmask 
          =   X.cWBackPixel 
          .|. X.cWEventMask 
          .|. X.cWBackingStore
          .|. X.cWBitGravity
    X.createWindow display root 
	 x y -- x, y
	 w h -- width, height
	 1   -- border_width
	 depth          -- use CopyFromParent??
	 X.inputOutput
	 visual         -- use CopyFromParent??
	 attrmask
	 attributes

  -- AC, 1/9/2000: Tell the window manager that we want to use the
  -- DELETE_WINDOW protocol
  delWinAtom <- X.internAtom display "WM_DELETE_WINDOW" False
  X.setWMProtocols display window [delWinAtom]
 
  X.setTextProperty display window name	X.wM_ICON_NAME 
  X.setTextProperty display window name	X.wM_NAME 
  X.mapWindow display window 
  X.raiseWindow display window

  text_gc <- X.createGC display window     
  X.setBackground display text_gc bg_color
  X.setForeground display text_gc fg_color

  pen_gc <- X.createGC display window     
  X.setBackground display pen_gc bg_color
  X.setForeground display pen_gc fg_color

  brush_gc <- X.createGC display window     
  X.setBackground display brush_gc bg_color
  X.setForeground display brush_gc fg_color

  redraw <- case redrawMode of
    Unbuffered -> return UnbufferedStuff
    DoubleBuffered -> do
      gc <- X.createGC display window
      X.setForeground display gc bg_color -- gc for clearing the screen
      ref_mbuffer <- newIORef Nothing
      return (BufferedStuff gc depth ref_mbuffer)

  win <- newWindow display window fg_color text_gc pen_gc brush_gc (corner,(w,h)) redraw tickRate

  -- It might be some time till we get back to the event loop
  -- so we try to process as many events as possible now.
  -- This is a bit of a hack and partly aimed at avoiding the bug that
  -- directDraw might try to draw something before the first expose event
  -- is processed.
  -- To make the hack even more effective, we wait a short time (allegedly
  -- 1uS) and synchronise before looking for the event.
  -- 
  -- NB:
  -- This whole thing is based on the implicit notion that the server thread
  -- is "lower priority" than the user threads.  That is, the server thread
  -- will only run when no user threads are runnable.  
  --
  -- Or, more concretely, only the server thread calls yield so it's safe 
  -- to call the minor_eloop (which doesn't yield or block) but not the
  -- major_eloop because, amongst other things, it may yield or block.
  X.waitForEvent display 1
  X.sync display False
  minor_eloop display

  return win

closeWindow w = do
  closeWindow' True w
  removeDeadWindows      -- bring out your dead

getWindowRect w = do
  mb_dc <- readMVar (ref_dc w)
  case mb_dc of 
    Just dc -> do 
       (pt,sz) <- readMVar (ref_rect dc)
       return (toPoint pt, toSize sz)
    Nothing -> 
       return ((0,0),(0,0)) -- ToDo?

-- main_eloop :: X.Display -> IO ()
-- main_eloop d =
--   X.allocaXEvent $ \ xevent -> do
--   let loop = do
-- --    X.sync d False -- wild attempt to fix the broken X connection problem
--     count <- X.pending d
--     if (count > 0) then do
-- --      X.sync d False -- wild attempt to fix the broken X connection problem
-- 	 X.nextEvent d xevent
-- 	 window <- X.get_Window xevent
-- 	 wnd <- findWindow window
-- 	 etype <- X.get_EventType xevent
-- --      print (window,etype)
-- 	 dispatchEvent wnd etype xevent
-- 	 ws <- readMVar wnds
-- 	 unless (null ws) loop
-- 	else 
-- 	 loop
--   loop

-- This is the main event loop in the program
main_eloop :: X.Display -> IO ()
main_eloop d =
  X.allocaXEvent $ \ xevent -> do
  let handleEvent = do
      count <- X.pending d
      next  <- T.nextTick timers
      if (count > 0 || not (isJust next))
       then do
         -- Event in queue or no tick pending.
	 X.nextEvent d xevent
	 window <- X.get_Window xevent
	 etype  <- X.get_EventType xevent
	 -- showEvent etype
	 withWindow window $ \ wnd -> do
	   dispatchEvent d wnd etype xevent
	else do
	 -- No event and tick pending.
	 let delay = fromJust next
	 t0 <- getTime
	 timedOut <- X.waitForEvent d (fromIntegral (delay * 1000))
	 t1 <- getTime
	 T.fireTimers timers (t1 - t0)
  let 
    loop = do
      -- We yield at this point because we're (potentially) 
      -- about to block so we should give other threads a chance
      -- to run.
      yield
      ws <- readMVar wnds
      if (null ws) 
        then return ()
        else do
          handleEvent 
          loop
  loop

-- This event loop is the same as above except that it is 
-- non-blocking: it only handles those events that have already arrived.
-- And this is important because it means we don't have to yield which
-- means it can safely be called by user code (see comment in openWindowEx).
minor_eloop :: X.Display -> IO ()
minor_eloop d =
  X.allocaXEvent $ \ xevent -> do
  let 
    handleEvent = do
      X.nextEvent d xevent
      window <- X.get_Window xevent
      etype  <- X.get_EventType xevent
  --       print etype
      withWindow window $ \ wnd -> do
        dispatchEvent d wnd etype xevent
      return ()

    loop = do
      ws <- readMVar wnds
      if null ws
        then 
          return ()
        else do
          -- Note: _do not_ call pending if null ws
          count <- X.pending d
          if count == 0
            then return ()
            else do
              handleEvent 
              loop
  loop

-- The DC is wrapped inside (MVar (Maybe ...)) so that we can mark
-- windows as being dead the moment they die and so that we don't
-- try to keep writing to them afterwards.
-- The events remain valid after the window dies.
-- It might be wiser to clear all events(???) and start returning
-- Closed whenever events are read - or (more GC friendly?), when
-- first read occurs but block thereafter?

data RedrawStuff 
  = UnbufferedStuff
  | BufferedStuff
      X.GC 			-- GC with foreground = background_color
      Int  			-- depth
      (IORef (Maybe X.Pixmap))	-- The buffer, allocated on demand
				-- drawBuffered.      

drawOnDC :: DC -> Draw () -> RedrawStuff -> IO ()
drawOnDC dc p redraw = 
  case redraw of
  UnbufferedStuff -> drawUnbuffered dc p
  BufferedStuff gc depth ref_mbuffer -> drawBuffered dc p gc depth ref_mbuffer

newWindow :: X.Display -> X.Window -> X.Pixel -> X.GC -> X.GC -> X.GC -> (X.Point,(X.Dimension,X.Dimension)) -> RedrawStuff -> Maybe Time -> IO Window
newWindow display window fg_color tgc pgc bgc rect redraw tickRate = do
  es  <- E.newEvents
  pic <- newMVar (return ())
-- failed attempts to find the default font
--  f'  <- X.fontFromGC display tgc
--  f   <- X.queryFont display f'
-- Since we can't ask the server what default font it chooses to bless
-- us with, we have to set an explicit font.  
  f   <- X.loadQueryFont display "9x15"  -- a random choice
  X.setFont display tgc (X.fontFromFontStruct f)
  bits <- newMVar DC_Bits
    { textColor = RGB 255 255 255
    , bkColor   = RGB 0   0   0
    , bkMode    = Transparent
    , textAlignment = (Left',Top)
    , brush     = Brush (RGB 255 255 255)
    , pen       = defaultPen fg_color
    , font      = Font f
    }
  ref_rect <- newMVar rect
  dc     <- newMVar (Just MkDC{disp=display,drawable=window,textGC=tgc,paintGC=pgc,brushGC=bgc,ref_rect=ref_rect,ref_bits=bits})
  timer <- case tickRate of
   	   Just t  -> T.new timers t (E.sendTick es) >>= return.Just
   	   Nothing -> return Nothing
  ref_exposed <- newIORef False
  let wnd = MkWindow{wnd=window,ref_dc=dc,exposed=ref_exposed,events=es,graphic=pic,redraw=redraw,timer=timer}
  Utils.modMVar wnds (wnd:)
  return wnd

redrawWindow w = do
  canDraw <- readIORef (exposed w)
  when canDraw $ do
    mb_dc <- readMVar (ref_dc w)
    case mb_dc of
      Just dc -> do
	p <- readMVar (graphic w)
	drawOnDC dc p (redraw w)
      Nothing -> return ()

directDraw w p = do
  mb_dc <- readMVar (ref_dc w)
  canDraw <- readIORef (exposed w)
  when canDraw $ do
    case mb_dc of
      Just dc -> unDraw p dc
      Nothing -> return ()

findWindow :: X.Window -> IO Window
findWindow xw = do
  ws <- readMVar wnds
  return (head [ w | w <- ws, xw == wnd w ])  -- ToDo: don't use head

withWindow :: X.Window -> (Window -> IO ()) -> IO ()
withWindow xw k = do
  ws <- readMVar wnds
  case [ w | w <- ws, xw == wnd w ] of
    (w:_) -> k w
    _     -> return ()

send :: Window -> Event -> IO ()
send w e = E.sendEvent (events w) e

dispatchEvent :: X.Display -> Window -> X.EventType -> X.XEventPtr -> IO ()
dispatchEvent display w etype xevent 
  | etype == X.graphicsExpose || etype == X.expose
  = paint
  | etype == X.motionNotify
  = mouseMove
  | etype == X.buttonPress
  = button True
  | etype == X.buttonRelease
  = button False
  | etype == X.keyPress
  = key True
  | etype == X.keyRelease
  = key False
  | etype == X.configureNotify
  = reconfig
  | etype == X.destroyNotify
  = destroy
  -- AC, 1/9/2000: treat a ClientMesage as a destroy event
  -- TODO: really need to examine the event in more detail,
  -- and ensure that xevent.xclient.message_type==ATOM_WM_PROTOCOLS &&
  --  xevent.xclient.data.l[0]==ATOM_WM_DELETE_WINDOW
  -- where ATOM_XXX is obtained from XInternAtom(dpy,"XXX",False)
  | etype == X.clientMessage
  = destroy

  -- ToDo: consider printing a warning message
  | otherwise
  = return ()

 where

  -- Redrawing is awkward because the request comes as a number of
  -- separate events.  We need to do one of the following (we currently
  -- do a combination of (1) and (3)):
  -- 1) Do a single redraw of the entire window but first delete all other
  --    expose events for this window from the queue.
  -- 2) Use all expose events for this window to build a Region object
  --    and use that to optimise redraws.
  -- 3) When double-buffering, use the buffer and information about
  --    whether it is up to date to serve redraws from the buffer.
  --    When single-buffering, use the server's backing store to reduce
  --    the number of expose events.  (Combine with bit-gravity info to
  --    handle resize requests.)
  paint :: IO ()
  paint = do
    let 
      stompOnExposeEvents = do
--        X.get_ExposeEvent xevent >>= print
        gotOne <- X.checkTypedWindowEvent display (wnd w) X.expose xevent
        when gotOne stompOnExposeEvents
    writeIORef (exposed w) True  -- now safe to draw directly
    stompOnExposeEvents
    p <- readMVar (graphic w)
    mb_dc <- readMVar (ref_dc w)
    case mb_dc of 
      Just dc -> drawOnDC dc p (redraw w)
      Nothing -> return ()

  button :: Bool -> IO ()
  button isDown = do
    (_,_,_,x,y,_,_,_,b,_) <- X.get_ButtonEvent xevent
    let isLeft = b == 1 -- assume that button 1 = left button
    send w Button{pt = (x,y), isLeft=isLeft, isDown=isDown}

-- An X KeySym is *not* a character; not even a Unicode character! And
-- since characters in Hugs only are 8-bit, we get a runtime error
-- below. There is an underlying assumption that key events only
-- involve characters. But of course there are function keys, arrow
-- keys, etc. too. While this will be a problem if one wants to get at
-- e.g. arrow keys (e.g. for some drawing application) or at
-- dead/multi-keys for doing proper input, we'll ignore them
-- completely for now. Furthermore, one really needs to call
-- XlookupString (not XkeysymToString!) to do the processing! We'll
-- ignore that too, and do a static mapping of just a few keysyms.

  key :: Bool -> IO ()
  key isDown =
    do
      -- Should really use XmbLookupString here to make compose work.
      -- It's OK to call X.lookupString both on key up and down events.
      -- Not true for X.mbLookupString. In that case, use e.g. X.lookup
      -- on key up events.
      (mks, s) <- X.lookupString (X.asKeyEvent xevent)
      case mks of
	Just ks -> send w (Key {keysym = MkKey ks, isDown = isDown})
	Nothing -> return ()
      if isDown then (mapM_ (\c -> send w (Char {char = c})) s) else return ()

  mouseMove ::IO ()
  mouseMove = do
    (_,_,_,x,y,_,_,_,_,_) <- X.get_MotionEvent xevent
    send w MouseMove{ pt = (x,y) }

  reconfig :: IO ()
  reconfig = do
        (x,y,width,height) <- X.get_ConfigureEvent xevent
    	mb_dc <- readMVar (ref_dc w)
    	case mb_dc of 
    	  Just dc -> do
	      Utils.modMVar (ref_rect dc) (const ((X.Point x y),(width,height)))
	      case (redraw w) of
	          UnbufferedStuff -> return ()
		  BufferedStuff _ _ ref_mbuffer -> removeBuffer dc ref_mbuffer
    	  Nothing -> return ()

	-- don't send new size, it may be out of date by the time we
	-- get round to reading the event
	send w Resize

  destroy :: IO ()
  destroy = do
	-- putStrLn "Window Destroyed" -- todo
        closeWindow' True w
        removeDeadWindows     -- bring out your dead
        send w Closed

----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

-- Only for debugging

showEvent :: X.EventType -> IO ()
showEvent etype
  | etype == X.keyPress
  = putStrLn "keyPress"
  | etype == X.keyRelease
  = putStrLn "keyRelease"
  | etype == X.buttonPress
  = putStrLn "buttonPress"
  | etype == X.buttonRelease
  = putStrLn "buttonRelease"
  | etype == X.motionNotify
  = putStrLn "motionNotify"
  | etype == X.enterNotify
  = putStrLn "enterNotify"
  | etype == X.leaveNotify
  = putStrLn "leaveNotify"
  | etype == X.focusIn
  = putStrLn "focusIn"
  | etype == X.focusOut
  = putStrLn "focusOut"
  | etype == X.keymapNotify
  = putStrLn "keymapNotify"
  | etype == X.expose
  = putStrLn "expose"
  | etype == X.graphicsExpose
  = putStrLn "graphicsExpose"
  | etype == X.noExpose
  = putStrLn "noExpose"
  | etype == X.visibilityNotify
  = putStrLn "visibilityNotify"
  | etype == X.createNotify
  = putStrLn "createNotify"
  | etype == X.destroyNotify
  = putStrLn "destroyNotify"
  | etype == X.unmapNotify
  = putStrLn "unmapNotify"
  | etype == X.mapNotify
  = putStrLn "mapNotify"
  | etype == X.mapRequest
  = putStrLn "mapRequest"
  | etype == X.reparentNotify
  = putStrLn "reparentNotify"
  | etype == X.configureNotify
  = putStrLn "configureNotify"
  | etype == X.configureRequest
  = putStrLn "configureRequest"
  | etype == X.gravityNotify
  = putStrLn "gravityNotify"
  | etype == X.resizeRequest
  = putStrLn "resizeRequest"
  | etype == X.circulateNotify
  = putStrLn "circulateNotify"
  | etype == X.circulateRequest
  = putStrLn "circulateRequest"
  | etype == X.propertyNotify
  = putStrLn "propertyNotify"
  | etype == X.selectionClear
  = putStrLn "selectionClear"
  | etype == X.selectionRequest
  = putStrLn "selectionRequest"
  | etype == X.selectionNotify
  = putStrLn "selectionNotify"
  | etype == X.colormapNotify
  = putStrLn "colormapNotify"
  | etype == X.clientMessage
  = putStrLn "clientMessage"
  | etype == X.mappingNotify
  = putStrLn "mappingNotify"
  | etype == X.lASTEvent
  = putStrLn "lASTEvent"
  | otherwise
  = putStrLn ("Unknown X event type: " ++ show etype)

----------------------------------------------------------------
-- End
----------------------------------------------------------------
