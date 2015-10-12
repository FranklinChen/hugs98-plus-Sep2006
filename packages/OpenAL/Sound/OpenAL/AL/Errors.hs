--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.Errors
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 2.7 (AL Errors) of the OpenAL
-- Specification and Reference (version 1.1).
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.Errors (
   ALError(..), ALErrorCategory(..), alErrors
) where

import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )
import Sound.OpenAL.AL.BasicTypes ( ALenum )
import Sound.OpenAL.Constants (
   al_NO_ERROR, al_INVALID_NAME, al_INVALID_ENUM, al_INVALID_VALUE,
   al_INVALID_OPERATION, al_OUT_OF_MEMORY )
import Sound.OpenAL.AL.QueryUtils ( StringName(..), getString )

--------------------------------------------------------------------------------

-- | AL errors consist of a general error category and a description of what
-- went wrong.

data ALError = ALError ALErrorCategory String
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

-- | General AL error categories.

data ALErrorCategory =
     ALInvalidEnum
   | ALInvalidValue
   | ALInvalidOperation
   | ALInvalidName
   | ALOutOfMemory
   deriving ( Eq, Ord, Show )

unmarshalALErrorCategory :: ALenum -> ALErrorCategory
unmarshalALErrorCategory x
   | x == al_INVALID_ENUM = ALInvalidEnum
   | x == al_INVALID_VALUE = ALInvalidValue
   | x == al_INVALID_OPERATION = ALInvalidOperation
   | x == al_INVALID_NAME = ALInvalidName
   | x == al_OUT_OF_MEMORY = ALOutOfMemory
   | otherwise = error ("unmarshalALErrorCategory: illegal value " ++ show x)

--------------------------------------------------------------------------------

-- | OpenAL detects only a subset of those conditions that could be considered
-- errors. This is because in many cases error checking would adversely impact
-- the performance of an error-free program. The state variable 'alErrors' is
-- used to obtain error information. When an error is detected by AL, a flag is
-- set and the error code is recorded. Further errors, if they occur, do not
-- affect this recorded code. When 'alErrors' is read, the error is returned and
-- the flag is cleared, so that a further error will again record its code. If
-- reading 'alErrors' returns @\[\]@ then there has been no detectable error
-- since the last time 'alErrors' (or since the AL was initialized).
--
-- When an error flag is set, results of AL operations are undefined only if
-- 'ALOutOfMemory' has occurred. In other cases, the command generating the
-- error is ignored so that it has no effect on AL state or output buffer
-- contents. If the error generating command returns a value, it returns zero.
-- If the generating command modifies values through a pointer argument, no
-- change is made to these values. These error semantics apply only to AL
-- errors, not to system errors such as memory access errors.

alErrors :: GettableStateVar [ALError]
alErrors =
   makeGettableStateVar $ do
      c <- alGetError
      if c == al_NO_ERROR
         then return []
         else do
            s <- getString (ALErrorCategory c)
            return [ ALError (unmarshalALErrorCategory c) s ]

foreign import CALLCONV unsafe "alGetError"
   alGetError :: IO ALenum
