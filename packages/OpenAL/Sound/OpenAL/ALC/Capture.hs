--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.ALC.Capture
-- Copyright   :  (c) Sven Panne 2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 6.4.2. (Capture) of the OpenAL
-- Specification and Reference (version 1.1).
--
--------------------------------------------------------------------------------

module Sound.OpenAL.ALC.Capture (
   NumSamples, captureOpenDevice, captureStart, captureNumSamples,
   captureSamples, captureStop, captureCloseDevice,
   captureDefaultDeviceSpecifier, captureDeviceSpecifier,
   allCaptureDeviceSpecifiers
) where

import Foreign.Ptr ( Ptr, nullPtr, FunPtr )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, get )
import Sound.OpenAL.AL.Buffer ( Format )
import Sound.OpenAL.AL.Format ( marshalFormat )
import Sound.OpenAL.ALC.ALCboolean ( unmarshalALCboolean )
import Sound.OpenAL.ALC.BasicTypes (
   ALCchar, ALCuint, ALCenum, ALCsizei, ALCboolean )
import Sound.OpenAL.ALC.Context ( Frequency )
import Sound.OpenAL.ALC.Device ( Device )
import Sound.OpenAL.ALC.Extensions ( alcProcAddress )
import Sound.OpenAL.ALC.QueryUtils ( IntQuery(..), getInteger )
import Sound.OpenAL.ALC.QueryUtils (
   StringQuery(..), getString, getStringRaw, alcIsExtensionPresent )
import Sound.OpenAL.ALC.String ( withALCString, peekALCStrings )
import Sound.OpenAL.Config ( ALCdevice, marshalDevice, unmarshalDevice )

--------------------------------------------------------------------------------

type NumSamples = ALCsizei

--------------------------------------------------------------------------------

type Invoker a = FunPtr a -> a

getCaptureFunc :: String -> IO (FunPtr a)
getCaptureFunc = get . alcProcAddress Nothing

--------------------------------------------------------------------------------

captureOpenDevice ::
   Maybe String -> Frequency -> Format -> NumSamples -> IO (Maybe Device)
captureOpenDevice maybeDeviceSpec frequency format size = do
   funPtr <- getCaptureFunc "alcCaptureOpenDevice"
   let open deviceSpec =
          invokeCaptureOpenDevice funPtr deviceSpec (round frequency)
                                  (fromIntegral (marshalFormat format)) size
   fmap unmarshalDevice $
      (maybe (open nullPtr)   -- use preferred device
             (flip withALCString open)
             maybeDeviceSpec)

foreign import CALLCONV unsafe "dynamic"
   invokeCaptureOpenDevice :: Invoker (Ptr ALCchar -> ALCuint -> ALCenum -> ALCsizei -> IO ALCdevice)

--------------------------------------------------------------------------------

captureStart :: Device -> IO ()
captureStart = captureStartStop "alcCaptureStart"

captureStartStop :: String -> Device -> IO ()
captureStartStop funName device = do
   funPtr <- getCaptureFunc funName
   invokeCaptureStartStop funPtr (marshalDevice device)

foreign import CALLCONV unsafe "dynamic"
   invokeCaptureStartStop :: Invoker (ALCdevice -> IO ()) 

--------------------------------------------------------------------------------

captureNumSamples :: Device -> GettableStateVar NumSamples
captureNumSamples device = makeGettableStateVar $
   fmap fromIntegral (getInteger (Just device) CaptureSamples)

--------------------------------------------------------------------------------

captureSamples :: Device -> Ptr a -> NumSamples -> IO ()
captureSamples device buf n = do
   funPtr <- getCaptureFunc "alcCaptureSamples"
   invokeCaptureSamples funPtr (marshalDevice device) buf n

foreign import CALLCONV unsafe "dynamic"
   invokeCaptureSamples :: Invoker (ALCdevice -> Ptr a -> NumSamples -> IO ())

--------------------------------------------------------------------------------

captureStop :: Device -> IO ()
captureStop = captureStartStop "alcCaptureStop"

--------------------------------------------------------------------------------

captureCloseDevice :: Device -> IO Bool
captureCloseDevice device = do
   funPtr <- getCaptureFunc "alcCaptureCloseDevice"
   fmap unmarshalALCboolean .
      invokeCaptureCloseDevice funPtr . marshalDevice $ device

foreign import CALLCONV unsafe "dynamic"
   invokeCaptureCloseDevice :: Invoker (ALCdevice -> IO ALCboolean) 

--------------------------------------------------------------------------------

-- | Contains the name of the default capture device.

captureDefaultDeviceSpecifier :: GettableStateVar String
captureDefaultDeviceSpecifier = makeGettableStateVar $
   getString Nothing CaptureDefaultDeviceSpecifier

--------------------------------------------------------------------------------

-- | Contains the specifier string for the given capture device.

captureDeviceSpecifier :: Device -> GettableStateVar String
captureDeviceSpecifier device = makeGettableStateVar $
   getString (Just device) CaptureDeviceSpecifier

--------------------------------------------------------------------------------

-- | Contains a list of specifiers for all available capture devices.

allCaptureDeviceSpecifiers :: GettableStateVar [String]
allCaptureDeviceSpecifiers = makeGettableStateVar $ do
   enumExtPresent <- get (alcIsExtensionPresent Nothing "ALC_ENUMERATION_EXT")
   if enumExtPresent
      then peekALCStrings =<< getStringRaw Nothing CaptureDeviceSpecifier
      else fmap (\s -> [s]) $ get captureDefaultDeviceSpecifier
