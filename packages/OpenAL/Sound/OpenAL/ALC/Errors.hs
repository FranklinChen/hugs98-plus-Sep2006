--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.ALC.Errors
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 6.3.6 (Query for Error Conditions) of the
-- OpenAL Specification and Reference (version 1.1).
--
--------------------------------------------------------------------------------

module Sound.OpenAL.ALC.Errors (
   ALCError(..), ALCErrorCategory(..), alcErrors
) where

import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )
import Sound.OpenAL.ALC.BasicTypes ( ALCenum )
import Sound.OpenAL.ALC.Device ( Device )
import Sound.OpenAL.ALC.QueryUtils ( StringQuery(..), getString )
import Sound.OpenAL.Constants (
   alc_NO_ERROR, alc_INVALID_DEVICE, alc_INVALID_CONTEXT, alc_INVALID_ENUM,
   alc_INVALID_VALUE, alc_OUT_OF_MEMORY, alc_INVALID_OPERATION )
import Sound.OpenAL.Config ( ALCdevice, marshalDevice )

--------------------------------------------------------------------------------

-- | ALC errors consist of a general error category and a description of what
-- went wrong.

data ALCError = ALCError ALCErrorCategory String
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

-- | General ALC error categories.

data ALCErrorCategory =
     ALCInvalidEnum
   | ALCInvalidValue
   | ALCInvalidOperation
   | ALCInvalidDevice
   | ALCInvalidContext
   | ALCOutOfMemory
   deriving ( Eq, Ord, Show )

unmarshalALCErrorCategory :: ALCenum -> ALCErrorCategory
unmarshalALCErrorCategory x
   | x == alc_INVALID_ENUM = ALCInvalidEnum
   | x == alc_INVALID_VALUE = ALCInvalidValue
   | x == alc_INVALID_OPERATION = ALCInvalidOperation
   | x == alc_INVALID_DEVICE = ALCInvalidDevice
   | x == alc_INVALID_CONTEXT = ALCInvalidContext
   | x == alc_OUT_OF_MEMORY = ALCOutOfMemory
   | otherwise = error ("unmarshalALCErrorCategory: illegal value " ++ show x)

--------------------------------------------------------------------------------

-- | OpenAL detects only a subset of those conditions that could be considered
-- errors. This is because in many cases error checking would adversely impact
-- the performance of an error-free program. The state variable 'alcErrors' is
-- used to obtain error information. When an error is detected by ALC, a flag is
-- set and the error code is recorded. Further errors, if they occur, do not
-- affect this recorded code. When 'alcErrors' is read, the error for the given
-- device is returned and the flag is cleared, so that a further error will
-- again record its code. If reading 'alcErrors' returns @\[\]@ then there has
-- been no detectable error since the last time 'alcErrors' (or since the ALC
-- was initialized).
--
-- When an error flag is set, results of ALC operations are undefined only if
-- 'ALCOutOfMemory' has occurred. In other cases, the command generating the
-- error is ignored so that it has no effect on ALC state or output buffer
-- contents. If the error generating command returns a value, it returns zero.
-- If the generating command modifies values through a pointer argument, no
-- change is made to these values. These error semantics apply only to ALC
-- errors, not to system errors such as memory access errors.

alcErrors :: Device -> GettableStateVar [ALCError]
alcErrors device =
   makeGettableStateVar $ do
      c <- alcGetError (marshalDevice device)
      if c == alc_NO_ERROR
         then return []
         else do
            s <- getString (Just device) (ALCErrorCategory c)
            return [ ALCError (unmarshalALCErrorCategory c) s ]

foreign import CALLCONV unsafe "alcGetError"
   alcGetError :: ALCdevice -> IO ALCenum
