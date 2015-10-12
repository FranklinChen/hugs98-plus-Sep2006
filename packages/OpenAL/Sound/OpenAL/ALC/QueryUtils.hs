-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.ALC.QueryUtils
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Sound.OpenAL.ALC.QueryUtils (
   StringQuery(..), getString, getStringRaw,
   IntQuery(..), marshalIntQuery, getInteger, getIntegerv,
   alcIsExtensionPresent
) where

import Control.Monad ( when )
import Foreign.Marshal.Array ( withArray, peekArray )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )
import Sound.OpenAL.ALC.ALCboolean ( unmarshalALCboolean )
import Sound.OpenAL.ALC.BasicTypes (
   ALCboolean, ALCchar, ALCint, ALCenum, ALCsizei )
import Sound.OpenAL.ALC.String ( withALCString, peekALCString )
import Sound.OpenAL.Config ( Device, ALCdevice, nullDevice, marshalDevice )
import Sound.OpenAL.Constants (
   alc_DEFAULT_DEVICE_SPECIFIER, alc_DEVICE_SPECIFIER, alc_EXTENSIONS,
   alc_CAPTURE_DEFAULT_DEVICE_SPECIFIER, alc_CAPTURE_DEVICE_SPECIFIER,
   alc_ATTRIBUTES_SIZE, alc_ALL_ATTRIBUTES, alc_MAJOR_VERSION,
   alc_MINOR_VERSION, alc_CAPTURE_SAMPLES )

--------------------------------------------------------------------------------

data StringQuery =
     DefaultDeviceSpecifier
   | DeviceSpecifier
   | Extensions
   | CaptureDefaultDeviceSpecifier
   | CaptureDeviceSpecifier
   | ALCErrorCategory ALCenum

marshalStringQuery :: StringQuery -> ALCenum
marshalStringQuery x = case x of
   DefaultDeviceSpecifier -> alc_DEFAULT_DEVICE_SPECIFIER
   DeviceSpecifier -> alc_DEVICE_SPECIFIER
   Extensions -> alc_EXTENSIONS
   CaptureDefaultDeviceSpecifier -> alc_CAPTURE_DEFAULT_DEVICE_SPECIFIER
   CaptureDeviceSpecifier -> alc_CAPTURE_DEVICE_SPECIFIER
   ALCErrorCategory e -> e

--------------------------------------------------------------------------------

getString :: Maybe Device -> StringQuery -> IO String
getString device query = getStringRaw device query >>= peekALCString

getStringRaw :: Maybe Device -> StringQuery -> IO (Ptr ALCchar)
getStringRaw maybeDevice =
   alcGetString (marshalMaybeDevice maybeDevice) . marshalStringQuery

marshalMaybeDevice :: Maybe Device -> ALCdevice
marshalMaybeDevice = marshalDevice . maybe nullDevice id

foreign import CALLCONV unsafe "alcGetString"
   alcGetString :: ALCdevice -> ALCenum -> IO (Ptr ALCchar)

--------------------------------------------------------------------------------

data IntQuery =
     AttributesSize
   | AllAttributes
   | MajorVersion
   | MinorVersion
   | CaptureSamples

marshalIntQuery :: IntQuery -> ALCenum
marshalIntQuery x = case x of
   AttributesSize -> alc_ATTRIBUTES_SIZE
   AllAttributes -> alc_ALL_ATTRIBUTES
   MajorVersion -> alc_MAJOR_VERSION
   MinorVersion -> alc_MINOR_VERSION
   CaptureSamples -> alc_CAPTURE_SAMPLES

--------------------------------------------------------------------------------

getInteger :: Maybe Device -> IntQuery -> IO ALCint
getInteger maybeDevice query = fmap head $ getIntegerv maybeDevice query 1

-- We are extremely careful below to avoid segfaults in case that there is no
-- current context, an invalid device, etc.
getIntegerv :: Maybe Device -> IntQuery -> ALCsizei -> IO [ALCint]
getIntegerv maybeDevice query numALCints =
   let n = fromIntegral numALCints
   in withArray (replicate n 0) $ \buf -> do
         when (numALCints > 0) $
            alcGetIntegerv (marshalMaybeDevice maybeDevice)
                           (marshalIntQuery query) numALCints buf
         peekArray n buf

foreign import CALLCONV unsafe "alcGetIntegerv"
   alcGetIntegerv :: ALCdevice -> ALCenum -> ALCsizei -> Ptr ALCint -> IO ()

--------------------------------------------------------------------------------

-- | To verify that a given extension is available for the current context and
-- the device it is associated with, use 'alcIsExtensionPresent'. For invalid
-- and unsupported string tokens it contains 'False'. Using 'Nothing' as the
-- device is acceptable. The extension name is not case sensitive: The
-- implementation will convert the name to all upper-case internally (and will
-- express extension names in upper-case).

alcIsExtensionPresent :: Maybe Device -> String -> GettableStateVar Bool
alcIsExtensionPresent maybeDevice extensionName = makeGettableStateVar $
   fmap unmarshalALCboolean $
      withALCString extensionName $
         alcIsExtensionPresent_ (marshalMaybeDevice maybeDevice)

foreign import CALLCONV unsafe "alcIsExtensionPresent"
   alcIsExtensionPresent_ :: ALCdevice -> Ptr ALCchar -> IO ALCboolean
