-- #hide
module Graphics.HGL.Win32.WND
	( WND, mkWND, openWND, closeWND, redrawWND
	, handleEvents, closeAllHWNDs
	, beginGraphics, endGraphics
	, wndRect
	, getHWND
	, drawWND
	) where

import Graphics.HGL.Units (Point)
import Graphics.HGL.Internals.Event( Event(..) )
import Graphics.HGL.Internals.Draw (Draw, unDraw)
import Graphics.HGL.Internals.Events( Events, sendEvent, sendTick )
import Graphics.HGL.Internals.Utilities(safeTry, Exception)
import Graphics.HGL.Win32.Draw( DrawFun, setDefaults, withDC )
import Graphics.HGL.Win32.Types( Key(MkKey), toPoint )

import Control.Concurrent( yield )
import Control.Monad(liftM2,when)
import Data.Bits
import Data.IORef
import Data.Maybe(isJust)
import System.IO.Unsafe(unsafePerformIO)

import Graphics.Win32
import System.Win32 (getModuleHandle)

----------------------------------------------------------------
-- Once a window has been closed, we want to detect any further
-- operations on the window - so all access is via a mutable Maybe
----------------------------------------------------------------

newtype WND = MkWND (IORef (Maybe HWND))

closeWND :: WND -> IO ()
closeWND wnd@(MkWND hwndref) = do
  mb_hwnd <- readIORef hwndref
  writeIORef hwndref Nothing    -- mark it as closed
  case mb_hwnd of
    Just hwnd -> do
      removeHWND hwnd   -- added by Ulf Norell <ulfn@cs.chalmers.se>
      yield             -- added by Ulf
      destroyWindow hwnd
    Nothing   -> return ()

getHWND :: WND -> IO HWND
getHWND (MkWND hwndref) = do
  mb_hwnd <- readIORef hwndref
  case mb_hwnd of 
    Just hwnd -> return hwnd
    Nothing   -> ioError (userError "Attempted to act on closed window")

redrawWND :: WND -> IO ()
redrawWND wnd = do
  hwnd <- getHWND wnd
  invalidateRect (Just hwnd) Nothing False

drawWND :: WND -> Draw () -> IO ()
drawWND wnd p = do
  hwnd <- getHWND wnd
  withDC (Just hwnd) (\ hdc -> setDefaults hdc >> unDraw p hdc)

wndRect :: WND -> IO (Point, Point)
wndRect wnd = do
  hwnd <- getHWND wnd
  (l,t,r,b) <- getClientRect hwnd
  return (toPoint (l,t), toPoint (r,b))

mkWND :: HWND -> IO WND
mkWND hwnd = fmap MkWND (newIORef (Just hwnd))

openWND :: String -> Maybe POINT -> Maybe POINT 
           -> Events      -- where to send the events
           -> DrawFun     -- how to redraw the picture
           -> Maybe MilliSeconds  -- time between timer ticks
           -> IO WND
openWND name pos size events draw tickRate = do
  checkInitialised
  clAss <- newClass
  hwnd <- createWND name wndProc pos size wS_OVERLAPPEDWINDOW Nothing
  show hwnd False
  updateWindow hwnd
  maybe (return ())
	(\ rate -> setWinTimer hwnd 1 rate >> return ())
	tickRate	
  fmap MkWND (newIORef (Just hwnd))
 where
  wndProc hwnd msg wParam lParam = do
    -- print msg
    rs <- safeTry $ do
                 r <- windowProc 
		      (sendEvent events) 
		      draw
		      (\ wParam -> sendTick events)
		      hwnd msg wParam lParam
                 r `seq` return r  -- force it inside the try!
    case rs of
      Right a -> return a
      Left  e -> uncaughtError e >> return 0  -- Let's hope this works ok

  show hwnd iconified = 
    if iconified 
    then do  
      showWindow hwnd sW_SHOWNORMAL -- open "iconified"
      return ()
    else do 
      showWindow hwnd sW_RESTORE    -- open "restored" (ie normal size)
      bringWindowToTop hwnd

-- Note that this code uses a single (static) MSG throughout the whole
-- system - let's hope this isn't a problem
handleEvents :: IO Bool -> IO ()
handleEvents userQuit = do
  -- first wait for a window to be created or for the user prog to quit
  -- this avoids the race condition that we might quit (for lack of
  -- any windows) before the user's thread has even had a chance to run.
  safeTry $
    while (fmap not (liftM2 (||) userQuit (fmap not noMoreWindows)))
      yield   -- Ulf uses this instead of handleEvent
  -- then wait for all windows to be shut down or user to quit
  safeTry $
    while (fmap not (liftM2 (||) userQuit systemQuit)) 
      handleEvent
  return ()
 where
  while p s = do { c <- p; if c then s >> while p s else return () }

handleEvent :: IO ()
handleEvent = do
  yield  -- always yield before any blocking operation
  nowin <- noMoreWindows
  when (not nowin) $ allocaMessage $ \ lpmsg -> do
    getMessage lpmsg Nothing
    translateMessage lpmsg
    dispatchMessage lpmsg
    return ()


----------------------------------------------------------------
-- The grotty details - opening WNDs, creating classes, etc
----------------------------------------------------------------

className = mkClassName "Graphics.HGL.Win32.WND"

newClass :: IO ATOM
newClass = do
  icon         <- loadIcon   Nothing iDI_APPLICATION
  cursor       <- loadCursor Nothing iDC_ARROW
  whiteBrush   <- getStockBrush wHITE_BRUSH
  mainInstance <- getModuleHandle Nothing
  atom <- registerClass (
	(cS_HREDRAW .|. cS_VREDRAW), -- redraw if window size Changes
	mainInstance,
	(Just icon),
	(Just cursor),
	(Just whiteBrush),
	Nothing,
	className)
  --return atom
  return (maybe undefined id atom)

createWND :: String -> WindowClosure -> Maybe POINT -> Maybe POINT 
		    -> WindowStyle -> Maybe HMENU -> IO HWND
createWND name wndProc posn size style menu = do
  mainInstance <- getModuleHandle Nothing
  mbSize <- calcSize size
  hwnd <- createWindowEx 
	    0 -- Win32.wS_EX_TOPMOST 
	    className
	    name
	    style
	    (fmap (fromIntegral.fst) posn)    -- x
	    (fmap (fromIntegral.snd) posn)    -- y
	    (fmap (fromIntegral.fst) mbSize)  -- w
	    (fmap (fromIntegral.snd) mbSize)  -- h
	    Nothing                           -- parent
	    menu
	    mainInstance
	    wndProc
  addHWND hwnd
  return hwnd
 where
  calcSize :: Maybe POINT -> IO (Maybe POINT)
  calcSize = 
    maybe (return Nothing)
          (\ (width, height) -> do
             (l,t,r,b) <- adjustWindowRect (0,0,width,height) style (isJust menu)
             return $ Just (r-l, b-t))

windowProc :: (Event -> IO ()) ->   	-- Event Handler
              DrawFun ->		-- Picture redraw
	      (WPARAM -> IO ()) ->	-- tick
	      (HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT)
windowProc send redraw tick hwnd msg wParam lParam
  | msg == wM_PAINT
  = paint
  | msg == wM_MOUSEMOVE
  = mouseMove lParam
  | msg == wM_LBUTTONDOWN || msg == wM_LBUTTONDBLCLK
  = button lParam True True
  | msg == wM_LBUTTONUP
  = button lParam True False
  | msg == wM_RBUTTONDOWN || msg == wM_RBUTTONDBLCLK
  = button lParam False True
  | msg == wM_RBUTTONUP
  = button lParam False False
  | msg == wM_KEYDOWN
  = key wParam True
  | msg == wM_KEYUP
  = key wParam False
  | msg == wM_CHAR
  = char wParam
  | msg == wM_TIMER
  = timer wParam
  | msg == wM_SIZE
  = resize
{-
  | msg == wM_MOUSEACTIVATE
  = do
      hwnd' <- setFocus hwnd
      if hwnd `eqHWND` hwnd' 
        then
          return mA_NOACTIVATE  -- already had input focus
        else
          return mA_ACTIVATEANDEAT
-}
  | msg == wM_DESTROY
  = destroy
  | otherwise
  = defWindowProc (Just hwnd) msg wParam lParam

 where
  paint :: IO LRESULT
  paint = paintWith hwnd (\hdc lpps -> do
	redraw hwnd hdc
	return 0
	)

  button :: LPARAM -> Bool -> Bool -> IO LRESULT
  button lParam isLeft isDown = do
	let (y,x) = lParam `divMod` 65536
	send (Button {pt = toPoint (x,y), isLeft=isLeft, isDown=isDown})
	return 0

  key :: WPARAM -> Bool -> IO LRESULT
  key wParam isDown = do
     	send (Key { keysym = MkKey wParam, isDown = isDown })
	-- by returning 1 we let it get translated into a char too
	return 1

  char :: WPARAM -> IO LRESULT
  char wParam = do
     	send (Char { char = toEnum (fromIntegral wParam) })
	return 0

  mouseMove :: LPARAM -> IO LRESULT
  mouseMove lParam = do
	let (y,x) = lParam `divMod` 65536
	send (MouseMove { pt = toPoint (x,y) })
	return 0

  timer :: WPARAM -> IO LRESULT
  timer wParam = do
	tick wParam
	return 0

  resize :: IO LRESULT
  resize = do
	-- don't send new size, it may be out of date by the time we
	-- get round to reading the event
	send Resize
	return 0

  destroy :: IO LRESULT
  destroy = do
        removeHWND hwnd
        send Closed
	return 0

paintWith :: HWND -> (HDC -> LPPAINTSTRUCT -> IO a) -> IO a
paintWith hwnd p =
  allocaPAINTSTRUCT $ \ lpps -> do
  hdc  <- beginPaint hwnd lpps
  a    <- p hdc lpps
  endPaint hwnd lpps
  return a

----------------------------------------------------------------
-- The open window list
----------------------------------------------------------------

-- It's very important that we close any windows - even if the 
-- Haskell application fails to do so (or aborts for some reason).
-- Therefore we keep a list of open windows and close them all at the
-- end.

-- persistent list of open windows
windows :: IORef [HWND]
windows = unsafePerformIO (newIORef [])

initialised :: IORef Bool
initialised = unsafePerformIO (newIORef False)

noMoreWindows :: IO Bool
noMoreWindows = fmap null (readIORef windows)

-- It's also important that we abort cleanly if an uncaught IOError
-- occurs - this flag keeps track of such things

hadUncaughtError :: IORef Bool
hadUncaughtError = unsafePerformIO (newIORef False)

-- We call this if an uncaught error has occured
uncaughtError :: Exception -> IO ()
uncaughtError e = do
  putStr "Uncaught Error: "
  print e  
  writeIORef hadUncaughtError True

systemQuit :: IO Bool
systemQuit = liftM2 (||) (readIORef hadUncaughtError) noMoreWindows

beginGraphics :: IO ()
beginGraphics = do
  closeAllHWNDs  -- just in case any are already open!
  writeIORef initialised True

checkInitialised :: IO ()
checkInitialised = do
  init <- readIORef initialised
  if init 
    then return ()
    else ioError (userError msg)
 where
  msg = "Graphics library uninitialised: perhaps you forgot to use runGraphics?"

endGraphics :: IO ()
endGraphics = do
  closeAllHWNDs
  writeIORef initialised False

closeAllHWNDs :: IO ()
closeAllHWNDs = do
  hwnds <- readIORef windows
  mapM_ destroyWindow hwnds
  writeIORef windows []
  writeIORef hadUncaughtError False -- clear the system


addHWND :: HWND -> IO ()
addHWND hwnd = do
  hwnds <- readIORef windows
  writeIORef windows (hwnd:hwnds)

-- remove a HWND from windows list
removeHWND :: HWND -> IO ()
removeHWND hwnd = do
  hwnds <- readIORef windows
  writeIORef windows (filter (/= hwnd) hwnds)

