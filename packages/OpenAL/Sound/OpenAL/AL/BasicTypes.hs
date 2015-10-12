--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.BasicTypes
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 2.2 (Primitive Types) of the OpenAL
-- Specification and Reference (version 1.1).
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.BasicTypes (
   -- * Primitive Types
   -- $PrimitiveTypes

   -- * Floating-Point Computation
   -- $FloatingPointComputation

   ALboolean, ALchar, ALbyte, ALubyte, ALshort, ALushort, ALint, ALuint,
   ALsizei, ALenum, ALfloat, ALdouble
) where

import Sound.OpenAL.Config

--------------------------------------------------------------------------------
-- $PrimitiveTypes
-- As OpenAL is meant to allow for seamless integration with OpenGL code if
-- needed, the OpenAL library primitive (scalar) data types mimic the OpenGL
-- data types. Guaranteed minimum sizes are stated for OpenGL data types, but
-- the actual choice of C data type is left to the implementation. All
-- implementations on a given binary architecture, however, must use a common
-- definition of these data types.
 
--------------------------------------------------------------------------------
-- $FloatingPointComputation
-- Any representable floating-point value is legal as input to an OpenAL command
-- that requires floating point data. The result of providing a value that is
-- not a floating point number to such a command is unspecified, but must not
-- lead to OpenAL being interrupted or terminated. In IEEE arithmetic, for
-- example, providing a negative zero or a denormalized number to an OpenAL
-- command yields predictable results, while providing a NaN or infinity yields
-- unspecified results.  Some calculations require division. In such cases
-- (including implied divisions required by vector normalizations), a division
-- by zero produces an unspecified result but must not lead to OpenAL
-- interruption or termination.
