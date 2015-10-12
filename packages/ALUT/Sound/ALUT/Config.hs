-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.ALUT.Config
-- Copyright   :  (c) Sven Panne 2006
-- License     :  BSD-style (see the file libraries/ALUT/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This purely internal module defines the platform-specific stuff which has
-- been figured out by configure.
--
--------------------------------------------------------------------------------

module Sound.ALUT.Config (
   alut_Init,
   alut_InitWithoutContext,
   alut_Exit,

   alut_GetError,
   alut_GetErrorString,

   alut_CreateBufferFromFile,
   alut_CreateBufferFromFileImage,
   alut_CreateBufferHelloWorld,
   alut_CreateBufferWaveform,

   alut_LoadMemoryFromFile,
   alut_LoadMemoryFromFileImage,
   alut_LoadMemoryHelloWorld,
   alut_LoadMemoryWaveform,

   alut_GetMIMETypes,

   alut_GetMajorVersion,
   alut_GetMinorVersion,

   alut_Sleep
) where

--------------------------------------------------------------------------------

#include "HsALUTConfig.h"

--------------------------------------------------------------------------------

import Foreign.C
import Foreign.Ptr
import Sound.OpenAL.AL.BasicTypes

#if ALUTINIT_VOID || !HAVE_ALUTINITWITHOUTCONTEXT || ALUTEXIT_VOID || !HAVE_ALUTSLEEP
import Sound.OpenAL.AL.ALboolean
#endif

#if !HAVE_ALUTCREATEBUFFERFROMFILE || !HAVE_ALUTCREATEBUFFERFROMFILEIMAGE || !HAVE_ALUTCREATEBUFFERHELLOWORLD || !HAVE_ALUTCREATEBUFFERWAVEFORM
import Sound.OpenAL.AL.BufferInternal
#endif

#if !HAVE_ALUTSLEEP && defined(__GLASGOW_HASKELL__)
import Control.Concurrent
#endif


--------------------------------------------------------------------------------

#if ALUTINIT_VOID

alut_Init :: Ptr CInt -> Ptr CString -> IO ALboolean
alut_Init argc argv = alutInit argc argv >> return (marshalALboolean True)

foreign import CALLCONV unsafe "alutInit"
   alutInit :: Ptr CInt -> Ptr CString -> IO ()

#else

foreign import CALLCONV unsafe "alutInit"
   alut_Init :: Ptr CInt -> Ptr CString -> IO ALboolean

#endif

--------------------------------------------------------------------------------

#if HAVE_ALUTINITWITHOUTCONTEXT

foreign import CALLCONV unsafe "alutInitWithoutContext"
   alut_InitWithoutContext :: Ptr CInt -> Ptr CString -> IO ALboolean

#else

alut_InitWithoutContext :: Ptr CInt -> Ptr CString -> IO ALboolean
alut_InitWithoutContext _ _ = return (marshalALboolean True)

#endif

--------------------------------------------------------------------------------

#if ALUTEXIT_VOID

alut_Exit :: IO ALboolean
alut_Exit = alutExit >> return (marshalALboolean True)

foreign import CALLCONV unsafe "alutExit"
   alutExit :: IO ()

#else

foreign import CALLCONV unsafe "alutExit"
   alut_Exit :: IO ALboolean

#endif

--------------------------------------------------------------------------------

#if HAVE_ALUTGETERROR

foreign import CALLCONV unsafe "alutGetError"
   alut_GetError :: IO ALenum

#else

alut_GetError :: IO ALenum
alut_GetError = return 0

#endif

--------------------------------------------------------------------------------

alut_GetErrorString :: ALenum -> IO String

#if HAVE_ALUTGETERRORSTRING

alut_GetErrorString e = peekCString =<< alutGetErrorString e

foreign import CALLCONV unsafe "alutGetErrorString"
   alutGetErrorString :: ALenum -> IO CString

#else

alut_GetErrorString _ = return "<unknown ALUT error>"

#endif

--------------------------------------------------------------------------------

#if HAVE_ALUTCREATEBUFFERFROMFILE

foreign import CALLCONV unsafe "alutCreateBufferFromFile"
   alut_CreateBufferFromFile :: CString -> IO ALuint

#else

alut_CreateBufferFromFile :: CString -> IO ALuint
alut_CreateBufferFromFile _ = return (marshalBuffer Nothing)

#endif

--------------------------------------------------------------------------------

#if HAVE_ALUTCREATEBUFFERFROMFILEIMAGE

foreign import CALLCONV unsafe "alutCreateBufferFromFileImage"
   alut_CreateBufferFromFileImage :: Ptr a -> ALsizei -> IO ALuint

#else

alut_CreateBufferFromFileImage :: Ptr a -> ALsizei -> IO ALuint
alut_CreateBufferFromFileImage _ _ =  return (marshalBuffer Nothing)

#endif

--------------------------------------------------------------------------------

#if HAVE_ALUTCREATEBUFFERHELLOWORLD

foreign import CALLCONV unsafe "alutCreateBufferHelloWorld"
   alut_CreateBufferHelloWorld :: IO ALuint

#else

alut_CreateBufferHelloWorld :: IO ALuint
alut_CreateBufferHelloWorld =  return (marshalBuffer Nothing)

#endif

--------------------------------------------------------------------------------

#if HAVE_ALUTCREATEBUFFERWAVEFORM

foreign import CALLCONV unsafe "alutCreateBufferWaveform"
   alut_CreateBufferWaveform :: ALenum -> ALfloat -> ALfloat -> ALfloat -> IO ALuint

#else

alut_CreateBufferWaveform :: ALenum -> ALfloat -> ALfloat -> ALfloat -> IO ALuint
alut_CreateBufferWaveform _ _ _ _ =  return (marshalBuffer Nothing)

#endif

--------------------------------------------------------------------------------

#if HAVE_ALUTLOADMEMORYFROMFILE

foreign import CALLCONV unsafe "alutLoadMemoryFromFile"
   alut_LoadMemoryFromFile :: CString -> Ptr ALenum -> Ptr ALsizei -> Ptr ALfloat -> IO (Ptr b)

#else

alut_LoadMemoryFromFile :: CString -> Ptr ALenum -> Ptr ALsizei -> Ptr ALfloat -> IO (Ptr b)
alut_LoadMemoryFromFile _ _ _ _ = return nullPtr

#endif

--------------------------------------------------------------------------------

#if HAVE_ALUTLOADMEMORYFROMFILEIMAGE

foreign import CALLCONV unsafe "alutLoadMemoryFromFileImage"
   alut_LoadMemoryFromFileImage :: Ptr a -> ALsizei -> Ptr ALenum -> Ptr ALsizei -> Ptr ALfloat -> IO (Ptr b)

#else

alut_LoadMemoryFromFileImage :: Ptr a -> ALsizei -> Ptr ALenum -> Ptr ALsizei -> Ptr ALfloat -> IO (Ptr b)
alut_LoadMemoryFromFileImage _ _ _ _ _ = return nullPtr

#endif

--------------------------------------------------------------------------------

#if HAVE_ALUTLOADMEMORYHELLOWORLD

foreign import CALLCONV unsafe "alutLoadMemoryHelloWorld"
   alut_LoadMemoryHelloWorld :: Ptr ALenum -> Ptr ALsizei -> Ptr ALfloat -> IO (Ptr b)

#else

alut_LoadMemoryHelloWorld :: Ptr ALenum -> Ptr ALsizei -> Ptr ALfloat -> IO (Ptr b)
alut_LoadMemoryHelloWorld _ _ _ = return nullPtr

#endif

--------------------------------------------------------------------------------

#if HAVE_ALUTLOADMEMORYWAVEFORM

foreign import CALLCONV unsafe "alutLoadMemoryWaveform"
   alut_LoadMemoryWaveform :: ALenum -> ALfloat -> ALfloat -> ALfloat -> Ptr ALenum -> Ptr ALsizei -> Ptr ALfloat -> IO (Ptr b)

#else

alut_LoadMemoryWaveform :: ALenum -> ALfloat -> ALfloat -> ALfloat -> Ptr ALenum -> Ptr ALsizei -> Ptr ALfloat -> IO (Ptr b)
alut_LoadMemoryWaveform _ _ _ _ _ _ _ = return nullPtr

#endif

--------------------------------------------------------------------------------

#if HAVE_ALUTGETMIMETYPES

foreign import CALLCONV unsafe "alutGetMIMETypes"
   alut_GetMIMETypes :: ALenum -> IO CString

#else

alut_GetMIMETypes :: ALenum -> IO CString
alut_GetMIMETypes _ = return nullPtr

#endif

--------------------------------------------------------------------------------

#if HAVE_ALUTGETMAJORVERSION

foreign import CALLCONV unsafe "alutGetMajorVersion"
   alut_GetMajorVersion :: IO ALint

#else

alut_GetMajorVersion :: IO ALint
alut_GetMajorVersion = return 0

#endif

--------------------------------------------------------------------------------

#if HAVE_ALUTGETMINORVERSION

foreign import CALLCONV unsafe "alutGetMinorVersion"
   alut_GetMinorVersion :: IO ALint

#else

alut_GetMinorVersion :: IO ALint
alut_GetMinorVersion = return 0

#endif

--------------------------------------------------------------------------------

#if HAVE_ALUTSLEEP

foreign import CALLCONV unsafe "alutSleep"
   alut_Sleep :: ALfloat -> IO ALboolean

#else

alut_Sleep :: ALfloat -> IO ALboolean
alut_Sleep d = mySleep d  >> return (marshalALboolean True)

mySleep :: ALfloat -> IO ()

#if defined(__GLASGOW_HASKELL__)

mySleep = threadDelay . round .  (* 1.0E6)

#else

mySleep = return ()

#endif

#endif
