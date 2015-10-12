--
-- Experiments with WinForms from Haskell
--
-- (c) 2002, Bryn Keller.
--
module Forms where

{-
  This example uses DotNet actions to access .NET
  rather than the FFI, as it was written before
  FFI support was added to hugs98.net. Feel free
  to upgrade it! :)
-}

import Dotnet

type Control a = Object a
type Config a = Control a -> IO ()

build :: IO ()
build = do
  frm <- mkCtrl "System.Windows.Forms.Form" [option setSize (200, 200)]
  btn <- mkCtrl "System.Windows.Forms.Button" [option setText "Click Me",
                                               option setSize (50,50),
                                               option setLocation (75,75)]
  frm `addCtrl` btn
  event btn "Click" (\_ _ -> msgBox "Hello!" "Congratulations, you're running Haskell code!")
  invokeStatic "System.Windows.Forms.Application" "Run" frm

option :: (Control a -> b -> IO()) -> b -> Config a
option f val = \ob -> f ob val

mkCtrl :: String -> [Config a] -> IO (Control a)
mkCtrl ctrlType options = do
  ctrl <- newObj ctrlType ()
  sequence_ (map (\x-> x ctrl) options)
  return ctrl

event :: Control a -> String -> (Object a -> Object b -> IO ()) -> IO()
event ctrl name func = do
  delegate <- newDelegator func
  () <- ctrl # invoke ("add_" ++ name) delegate
  return ()

setSize :: Control a -> (Int, Int) -> IO ()
setSize ctrl (width, height) = do
  bWidth <- boxValue width
  bHeight <- boxValue height
  () <- ctrl # invoke "set_Width" bWidth
  () <- ctrl # invoke "set_Height" bHeight
  return ()

setText :: Control a -> String -> IO ()
setText ctrl text = do
  () <- ctrl # invoke "set_Text" text
  return ()

setLocation :: Control a -> (Int,  Int) -> IO ()
setLocation ctrl (x,y) = do
  bX <- boxValue x
  bY <- boxValue y
  () <- ctrl # invoke "set_Left" bX
  () <- ctrl # invoke "set_Top" bY
  return ()


add :: Object a -> Object a -> IO ()
add collection thing = do
  () <- collection # invoke "Add" thing
  return ()

addCtrl :: Control a -> Control a -> IO ()
addCtrl parent child = do
  ctrls <- getControls parent
  () <- add ctrls child
  return ()

getControls :: Control a -> IO (Object a)
getControls frm = do
  ctrls <- frm # invoke "get_Controls" ()
  return ctrls

