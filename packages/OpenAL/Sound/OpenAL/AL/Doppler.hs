--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.Doppler
-- Copyright   :  (c) Sven Panne 2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.5.2. (Velocity Dependent Doppler Effect)
-- of the OpenAL Specification and Reference (version 1.1).
-- 
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.Doppler (
   -- * Introduction
   -- $Introduction
   dopplerFactor, speedOfSound
) where

import Foreign.Ptr ( FunPtr )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), StateVar, makeStateVar )
import Sound.OpenAL.AL.BasicTypes ( ALenum, ALfloat )
import Sound.OpenAL.AL.Extensions ( alProcAddress )
import Sound.OpenAL.AL.QueryUtils (
   GetPName(GetDopplerFactor,GetSpeedOfSound), marshalGetPName )

#ifdef __HADDOCK__
import Sound.OpenAL.AL.Errors ( ALError(ALInvalidValue) )
#endif

--------------------------------------------------------------------------------

-- | 'dopplerFactor' is a simple scaling of source and listener velocities to
-- exaggerate or deemphasize the Doppler (pitch) shift resulting from the
-- calculation. Setting 'dopplerFactor' to a negative value will result in an
-- 'ALInvalidValue' error, the command is then ignored. The default value is 1.
-- The implementation is free to optimize the case of 'dopplerFactor' containing
-- zero, as this effectively disables the effect.

dopplerFactor :: StateVar ALfloat
dopplerFactor = makeDopplerVar GetDopplerFactor "alDopplerFactor"

--------------------------------------------------------------------------------

-- | 'speedOfSound' allows the application to change the reference (propagation)
-- speed used in the Doppler calculation. The source and listener velocities
-- should be expressed in the same units as the speed of sound. Setting
-- 'speedOfSound' to a negative or zero value will result in an 'ALInvalidValue'
-- error, the command is ignored then. The default value is 343.3 (appropriate
-- for velocity units of meters and air as the propagation medium).

speedOfSound :: StateVar ALfloat
speedOfSound = makeDopplerVar GetSpeedOfSound "alSpeedOfSound"

--------------------------------------------------------------------------------

makeDopplerVar :: GetPName -> String -> StateVar ALfloat
makeDopplerVar p apiEntryName =
   makeStateVar
      (alGetFloat (marshalGetPName p))
      (\value -> do
         -- ToDo: Should we check alcVersion or alIsExtensionPresent here?
         funPtr <- get (alProcAddress apiEntryName)
         invokeWithFloat funPtr value)

foreign import CALLCONV unsafe "alGetFloat"
   alGetFloat :: ALenum -> IO ALfloat

type Invoker a = FunPtr a -> a

foreign import CALLCONV unsafe "dynamic"
   invokeWithFloat :: Invoker (ALfloat -> IO ()) 

--------------------------------------------------------------------------------
-- $Introduction
-- The Doppler Effect depends on the velocities of source and listener relative
-- to the medium, and the propagation speed of sound in that medium.  The
-- application might want to emphasize or de-emphasize the Doppler Effect as
-- physically accurate calculation might not give the desired results.
-- The amount of frequency shift (pitch change) is proportional to the speed of
-- listener and source along their line of sight.
-- 
-- The Doppler Effect as implemented by OpenAL is described in detail in section
-- 3.5.2 of the OpenAL 1.1 specification. Note that effects of the medium (air,
-- water) moving with respect to listener and source are ignored. There are two
-- API calls global to the current context that provide control of the Doppler
-- factor and the speed of sound. Distance and velocity units are completely
-- independent of one another (so you could use different units for each if
-- desired).
