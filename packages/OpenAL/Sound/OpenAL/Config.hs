-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.Config
-- Copyright   :  (c) Sven Panne 2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This purely internal module defines the platform-specific stuff which has
-- been figured out by configure.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.Config (
   -- AL types
   ALboolean, ALchar, ALbyte, ALubyte, ALshort, ALushort, ALint, ALuint,
   ALsizei, ALenum, ALfloat, ALdouble,

   -- ALC types
   ALCboolean, ALCchar, ALCbyte, ALCubyte, ALCshort, ALCushort, ALCint, ALCuint,
   ALCsizei, ALCenum, ALCfloat, ALCdouble,

   -- Device stuff
   ALCdevice, Device, nullDevice, marshalDevice, unmarshalDevice, closeDevice,

   -- Context stuff
   ALCcontext, Context, nullContext, marshalContext, unmarshalContext,
   alcProcessContext, alcMakeContextCurrent, alcDestroyContext
) where

import Data.Int
import Data.Word
import Foreign.Ptr ( Ptr, nullPtr )

--------------------------------------------------------------------------------

#include "HsOpenALConfig.h"

--------------------------------------------------------------------------------
-- AL types

-- | 8-bit boolean
type ALboolean = HTYPE_ALBOOLEAN

-- | Character
type ALchar = HTYPE_ALCHAR

-- | Signed 8-bit 2\'s complement integer
type ALbyte = HTYPE_ALBYTE

-- | Unsigned 8-bit integer
type ALubyte = HTYPE_ALUBYTE

-- | Signed 16-bit 2\'s complement integer
type ALshort = HTYPE_ALSHORT

-- | Unsigned 16-bit integer
type ALushort = HTYPE_ALUSHORT

-- | Signed 32-bit 2\'s complement integer
type ALint = HTYPE_ALINT

-- | Unsigned 32-bit integer
type ALuint = HTYPE_ALUINT

-- | Non-negatitve 32-bit binary integer size
type ALsizei = HTYPE_ALSIZEI

-- | Enumerated 32-bit value
type ALenum = HTYPE_ALENUM

-- | 32-bit IEEE754 floating-point
type ALfloat = HTYPE_ALFLOAT

-- | 64-bit IEEE754 floating-point
type ALdouble = HTYPE_ALDOUBLE

--------------------------------------------------------------------------------
-- ALC types

-- | 8-bit boolean
type ALCboolean = HTYPE_ALCBOOLEAN

-- | Character
type ALCchar = HTYPE_ALCCHAR

-- | Signed 8-bit 2\'s complement integer
type ALCbyte = HTYPE_ALCBYTE

-- | Unsigned 8-bit integer
type ALCubyte = HTYPE_ALCUBYTE

-- | Signed 16-bit 2\'s complement integer
type ALCshort = HTYPE_ALCSHORT

-- | Unsigned 16-bit integer
type ALCushort = HTYPE_ALCUSHORT

-- | Signed 32-bit 2\'s complement integer
type ALCint = HTYPE_ALCINT

-- | Unsigned 32-bit integer
type ALCuint = HTYPE_ALCUINT

-- | Non-negatitve 32-bit binary integer size
type ALCsizei = HTYPE_ALCSIZEI

-- | Enumerated 32-bit value
type ALCenum = HTYPE_ALCENUM

-- | 32-bit IEEE754 floating-point
type ALCfloat = HTYPE_ALCFLOAT

-- | 64-bit IEEE754 floating-point
type ALCdouble = HTYPE_ALCDOUBLE

--------------------------------------------------------------------------------
-- In OpenAL 1.1, alcCloseDevice() returns an ALCboolean, before it was void.
-- To break a dependency cycle, we have to define the Device type here, too.

-- | The abstract device type.

newtype Device = Device ALCdevice
   deriving ( Eq, Ord, Show )

newtype ALCdevice = ALCdevice (Ptr ALCdevice)
   deriving ( Eq, Ord, Show )

nullDevice :: Device
nullDevice = Device (ALCdevice nullPtr)

marshalDevice :: Device -> ALCdevice
marshalDevice (Device device) = device

unmarshalDevice :: ALCdevice -> Maybe Device
unmarshalDevice device =
   if device == marshalDevice nullDevice then Nothing else Just (Device device)

-- | 'closeDevice' allows the application (i.e. the client program) to
-- disconnect from a device (i.e. the server). It returns 'True' for success and
-- 'False' for failure. Once closed, the 'Device' is invalid.
--
-- /Note:/ Older OpenAL implementations will always report a success!

closeDevice :: Device -> IO Bool

#if ALCCLOSEDEVICE_VOID

closeDevice = fmap (const True) . alcCloseDevice . marshalDevice

foreign import CALLCONV unsafe "alcCloseDevice"
   alcCloseDevice :: ALCdevice -> IO ()

#else

-- inlined unmarshalALCboolean here to break dependency cycle
closeDevice = fmap (/= CONST_ALC_FALSE) . alcCloseDevice . marshalDevice

foreign import CALLCONV unsafe "alcCloseDevice"
   alcCloseDevice :: ALCdevice -> IO ALCboolean

#endif

--------------------------------------------------------------------------------
-- In OpenAL 1.1, alcProcessContext() returns void for all platforms, before it
-- returned ALCcontext* on Linux. To break a dependency cycle, we have to define
-- the Context type here, too.

-- | The abstract context type.

data Context = Context ALCcontext
   deriving ( Eq, Ord, Show )

newtype ALCcontext = ALCcontext (Ptr ALCcontext)
   deriving ( Eq, Ord, Show )

nullContext :: Context
nullContext = Context (ALCcontext nullPtr)

marshalContext :: Context -> ALCcontext
marshalContext (Context context) = context

unmarshalContext :: ALCcontext -> Maybe Context
unmarshalContext context =
   if context == marshalContext nullContext then Nothing else Just (Context context)

#if ALCPROCESSCONTEXT_VOID

foreign import CALLCONV unsafe "alcProcessContext"
   alcProcessContext :: ALCcontext -> IO ()

#else

foreign import CALLCONV unsafe "alcProcessContext"
   alcProcessContext :: ALCcontext -> IO ALCcontext

#endif

--------------------------------------------------------------------------------
-- In OpenAL 1.1, alcMakeContextCurrent() returns void, before it was ALCenum on
-- Linux and ALCboolean on other platforms. Currently we default to ALCenum in
-- the latter case.

#if ALCMAKECONTEXTCURRENT_VOID

foreign import CALLCONV unsafe "alcMakeContextCurrent"
   alcMakeContextCurrent :: ALCcontext -> IO ()

#else

foreign import CALLCONV unsafe "alcMakeContextCurrent"
   alcMakeContextCurrent :: ALCcontext -> IO ALCenum

#endif

--------------------------------------------------------------------------------
-- In OpenAL 1.1, alcDestroyContext() returns void, before it returned ALCenum
-- on Linux.

#if ALCDESTROYCONTEXT_VOID

foreign import CALLCONV unsafe "alcDestroyContext"
   alcDestroyContext :: ALCcontext -> IO ()

#else

foreign import CALLCONV unsafe "alcDestroyContext"
   alcDestroyContext :: ALCcontext -> IO ALCenum

#endif
