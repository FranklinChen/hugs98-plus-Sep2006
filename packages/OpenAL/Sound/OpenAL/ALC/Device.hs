--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.ALC.Device
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 6.1 (Managing Devices) of the OpenAL
-- Specification and Reference (version 1.1).
--
-- ALC introduces the notion of a device. A device can be, depending on the
-- implementation, a hardware device, or a daemon\/OS service\/actual
-- server. This mechanism also permits different drivers (and hardware) to
-- coexist within the same system, as well as allowing several applications to
-- share system resources for audio, including a single hardware output
-- device. The details are left to the implementation, which has to map the
-- available backends to unique device specifiers.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.ALC.Device (
   Device, openDevice, closeDevice,
   defaultDeviceSpecifier, deviceSpecifier, allDeviceSpecifiers
) where

import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Marshal.Utils ( maybePeek )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, get )
import Sound.OpenAL.ALC.BasicTypes ( ALCchar )
import Sound.OpenAL.ALC.QueryUtils (
   StringQuery(..), getString, getStringRaw, alcIsExtensionPresent )
import Sound.OpenAL.ALC.String ( withALCString, peekALCString, peekALCStrings )
import Sound.OpenAL.Config (
   Device, ALCdevice, unmarshalDevice, closeDevice )

--------------------------------------------------------------------------------

-- | 'openDevice' allows the application (i.e. the client program) to connect to
-- a device (i.e. the server). If the function returns 'Nothing', then no sound
-- driver\/device has been found. The argument to 'openDevice' specifies a
-- certain device or device configuration. If it is 'Nothing', the
-- implementation will provide an implementation specific default, see
-- 'defaultDeviceSpecifier'. Otherwise it is 'Just' an implementation-dependent
-- string. You can use 'allDeviceSpecifiers' to get a list of the known OpenAL
-- devices.
--
-- /Notes for Windows:/ There are 3 possible device strings, each having a
-- deprecated equivalent for legacy applications only:
--
-- * @\"Generic Hardware\"@ (legacy string: @\"DirectSound3D\"@)
--
-- * @\"Generic Software\"@ (legacy string: @\"DirectSound\"@)
--
-- * @\"Generic Software Fallback\"@ (legacy string: @\"MMSYSTEM\"@)
--
-- /Notes for Linux\/\*nix:/ If an @.openalrc@ file is present in the user\'s
-- home directory, it is loaded first, otherwise @\/etc\/openalrc@ is tried.
-- The bindings (if any) of @devices@, @direction@, @sampling-rate@, and
-- @speaker-num@ (see below) after loading one of these files take precedence
-- over any bindings done via the argument to 'openDevice'.
--
-- The syntax of these files is lisp-based and a sequence of expressions, where
-- an expression is one the following:
--
-- * A boolean value of the form @\#f@ or @\#t@, which evaluate to /false/ and
-- /true/, respectively.
--
-- * An non-negative integer value, i.e. a sequence of decimal digits,
-- evaluating to itself.
--
-- * A (signed) floating point value, as recoginzed by C\'s @atof()@, evaluating
-- to itself.
--
-- * A pointer value of the form @\#p/num/@, where /num/ can be an octal,
-- decimal or hexadecimal value, as recoginzed by C\`s @strtol()@, evaluating
-- to itself. This kind of expression is currently unused.
--
-- * A string, i.e. a sequence of printable\/whitespace characters between
-- double quotes, evaluating to itself.
--
-- * A symbol, i.e. a sequence of almost all characters which don\'t form a
-- simple expression like the ones mentioned below, e.g. @foo@, @bar1@, @3baz@,
-- ... The symbol evaluates to the value currently bound to it.
--
-- * A function application of the form @(/symbol/ /expression/...)@. The
-- function bound to the symbol is applied to the evaluated arguments.
-- 
-- * A quotation of the form @(quote /expression/)@ or @\'/expression/@,
-- evaluating to the unevaluated /expression/ itself.
--
-- * A definition of the form @(define /symbol/ /expression/)@, binding /symbol/
-- to the value of /expression/. The whole expression evaluates to the value of
-- /expression/, too.
--
-- * A conjunction of boolean expressions of the form @(and /expression/...)@.
-- Each /expression/ is evaluated in turn, and if one of them evaluates to
-- /false/, the value of the whole expression is /false/. Otherwise the value is
-- /true/.
--
-- * An extension loading mechanism of the form @(load-extension
-- /libraryName/)@, where /libraryName/ has to evaluate to a string. This tries
-- to load the dynamic library with up to 3 special entry points:
-- @alExtension_03282000@ (pointing to a mandatory NULL-terminated sequence of
-- pairs of pointers to names and extension functions), @alExtInit_03282000@ (an
-- optional initialization function), and @alExtFini_03282000@ (an optional
-- cleanup function). If the extension could be loaded successfully, the whole
-- expression evaluates to /true/, otherwise to /false/.
--
-- Some symbols have a special meaning for OpenAL:
--
-- [@devices@] Has the form @(/devspec/...)@, where /devspec/ is either a
-- symbol\/string specifying a device or @(/device/ /device-param/...)@,
-- specifying a device with additional parameters. These optional device
-- parameters are stored in a variable @device-params@, but are currently
-- unused. The first device which can successfully opened is used.
--
-- [@direction@] Type string or symbol: @\"read\"@ specifies that the device
-- should be an input device, everything else means output device (default).
--
-- [@sampling-rate@] Type integer or float: Specifies the internal mixing
-- frequency, default is 44.1kHz.
--
-- [@speaker-num@] Type integer or float: Specifies the number of speakers,
-- which can be 1, 2 (default), or 4.
--
-- [@alsa-device@] Type string, @alsa@ backend only: Specifies both
-- @alsa-out-device@ and @alsa-in-device@, default @\"plughw:0,0\"@.
-- 
-- [@alsa-out-device@] Type string, @alsa@ backend only: Specifies the ALSA
-- output device, defaults to the value of @alsa-device@.
--
-- [@alsa-in-device@] Type string, @alsa@ backend only: Specifies the ALSA
-- input device, defaults to the value of @alsa-device@.
--
-- [@native-in-device@] Type string, @native@ backend on IRIX only.
--
-- [@native-out-device@] Type string, @native@ backend on IRIX only.
--
-- [@native-rear-out-device@] Type string, @native@ backend on IRIX only.
--
-- [@native-use-select@] Type boolean, @native@ backend on Linux only: If @\#t@,
-- wait up to 0.8sec for the device to become ready for writing. If @\#f@, just
-- try to write and hope it won\'t hang forever. The latter might be necessary
-- for some drivers which don\'t implement @select()@ , like some Aureal
-- drivers.
--
-- [@lin-dsp-path@] Type string, @native@ backend on Linux only: Path to DSP
-- device for writing, tried before @\/dev\/sound\/dsp@ and @\/dev\/sound@ if
-- set.
--
-- [@lin-dsp-read-path@] Type string, @native@ backend on Linux only: Path to
-- DSP device for reading, tried before @\/dev\/sound\/dsp@ and @\/dev\/sound@
-- if set. Defaults to the value of @lin-dsp-path@.
--
-- [@native-backend-debug@] Type boolean, @native@ backend on Darwin only: If
-- set to @\#f@, be a bit verbose on stderr about what\'s going on in the
-- backend.
--
-- [@source-rolloff-factor@] Type integer or float: Value of the initial rolloff
-- factor for sources, default is @1.0@.
--
-- [@listener-position@] List of 3 integers or floats: Value of the initial
-- listener position, default is @(0 0 0)@.
--
-- [@listener-velocity@] List of 3 integers or floats: Value of the initial
-- listener velocity, default is @(0 0 0)@.
--
-- [@listener-orientation@] List of 6 integers or floats: Value of the initial
-- listener orientation (at\/up), default is @(0 0 -1 0 1 0)@.
--
-- The string given to 'openDevice' has to be of the form @\'((/symbol/
-- /expression/) ...)@, which means basically a sequence of @define@
-- expressions. Example:
--
-- @
-- \"\'((sampling-rate 8000) (native-backend-debug \#f))\"
-- @
--
-- /Note:/ The information above has been reverse-engineered from the OpenAL SI
-- and could be inaccurate. Any corrections and\/or additions are highly
-- welcome.

openDevice :: Maybe String -> IO (Maybe Device)
openDevice maybeDeviceSpec =
   fmap unmarshalDevice $
      maybe (alcOpenDevice nullPtr)   -- use preferred device
            (flip withALCString alcOpenDevice)
            maybeDeviceSpec

foreign import CALLCONV unsafe "alcOpenDevice"
   alcOpenDevice :: Ptr ALCchar -> IO ALCdevice

--------------------------------------------------------------------------------

-- | Contains 'Just' the specifier string for the default device or 'Nothing' if
-- there is no sound support at all.

defaultDeviceSpecifier :: GettableStateVar (Maybe String)
defaultDeviceSpecifier = makeGettableStateVar $
   getStringRaw Nothing DefaultDeviceSpecifier >>= maybePeek peekALCString

--------------------------------------------------------------------------------

-- | Contains the specifier string for the given device.

deviceSpecifier :: Device -> GettableStateVar String
deviceSpecifier device = makeGettableStateVar $
   getString (Just device) DeviceSpecifier

--------------------------------------------------------------------------------

-- | Contains a list of specifiers for all available devices.

allDeviceSpecifiers :: GettableStateVar [String]
allDeviceSpecifiers = makeGettableStateVar $ do
   enumExtPresent <- get (alcIsExtensionPresent Nothing "ALC_ENUMERATION_EXT")
   if enumExtPresent
      then getStringRaw Nothing DeviceSpecifier >>= peekALCStrings
      else get defaultDeviceSpecifier >>= (return . maybe [] (\s -> [s]))
