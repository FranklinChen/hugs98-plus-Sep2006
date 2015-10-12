--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.Listener
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to sections 4.1 (Basic Listener and Source
-- Attributes) and 4.2 (Listener Object) of the OpenAL Specification and
-- Reference (version 1.1).
-- 
-- The listener object defines various properties that affect processing of the
-- sound for the actual output. The listener is unique for an OpenAL Context,
-- and has no name. By controlling the listener, the application controls the
-- way the user experiences the virtual world, as the listener defines the
-- sampling\/pickup point and orientation, and other parameters that affect the
-- output stream.
-- 
-- It is entirely up to the driver and hardware configuration, i.e. the
-- installation of OpenAL as part of the operating system and hardware setup,
-- whether the output stream is generated for headphones or 2 speakers, 4.1
-- speakers, or other arrangements, whether (and which) HRTFs are applied,
-- etc...
--
---------------------------------------------------------------------------------

module Sound.OpenAL.AL.Listener (
   listenerPosition, listenerVelocity, Gain, listenerGain, orientation
) where

import Foreign.Marshal.Array ( allocaArray, withArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Vector3(..) )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Vertex3(..) )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )
import Sound.OpenAL.AL.BasicTypes ( ALenum, ALfloat )
import Sound.OpenAL.AL.PeekPoke ( peek1, peek3, peek6 )
import Sound.OpenAL.AL.QueryUtils (
   GetPName(GetPosition,GetVelocity,GetGain,GetOrientation), marshalGetPName )

--------------------------------------------------------------------------------

-- | 'listenerPosition' contains the current location of the listener in the
-- world coordinate system. Any 3-tuple of valid float values is allowed.
-- Implementation behavior on encountering NaN and infinity is not defined. The
-- initial position is ('Vertex3' 0 0 0).

listenerPosition :: StateVar (Vertex3 ALfloat)
listenerPosition = makeListenerVar GetPosition 3 (peek3 Vertex3) listener3f

--------------------------------------------------------------------------------

-- | 'listenerVelocity' contains current velocity (speed and direction) of the
-- listener in the world coordinate system. Any 3-tuple of valid float
-- values is allowed, and the initial velocity is ('Vector3' 0 0 0).
-- 'listenerVelocity' does not affect 'listenerPosition'. OpenAL does not
-- calculate the velocity from subsequent position updates, nor does it
-- adjust the position over time based on the specified velocity. Any
-- such calculation is left to the application. For the purposes of sound
-- processing, position and velocity are independent parameters affecting
-- different aspects of the sounds.
--
-- 'listenerVelocity' is taken into account by the driver to synthesize the
-- Doppler effect perceived by the listener for each source, based on the
-- velocity of both source and listener, and the Doppler related parameters.

listenerVelocity :: StateVar (Vector3 ALfloat)
listenerVelocity = makeListenerVar GetVelocity 3 (peek3 Vector3) listener3f

--------------------------------------------------------------------------------

-- | A scalar amplitude multiplier.
type Gain = ALfloat

-- | 'listenerGain' contains a scalar amplitude multiplier, which is effectively
-- applied to all sources in the current context. The initial value 1 means
-- that the sound is unattenuated. A 'listenerGain' value of 0.5 is equivalent
-- to an attenuation of 6dB. The value zero equals silence (no output). Driver
-- implementations are free to optimize this case and skip mixing and processing
-- stages where applicable. The implementation is in charge of ensuring
-- artifact-free (click-free) changes of gain values and is free to defer actual
-- modification of the sound samples, within the limits of acceptable latencies.
--
-- A 'listenerGain' larger than 1 (amplification) is permitted. However, the
-- implementation is free to clamp the total gain (effective gain per source
-- times listener gain) to 1 to prevent overflow.

listenerGain :: StateVar Gain
listenerGain = makeListenerVar GetGain 1 (peek1 id) listenerf

--------------------------------------------------------------------------------

-- | 'orientation' contains an \"at\" vector and an \"up\" vector, where the
-- \"at\" vector represents the \"forward\" direction of the listener and the
-- orthogonal projection of the \"up\" vector into the subspace perpendicular to
-- the \"at\" vector represents the \"up\" direction for the listener. OpenAL
-- expects two vectors that are linearly independent. These vectors are not
-- expected to be normalized. If the two vectors are linearly dependent,
-- behavior is undefined. The initial orientation is ('Vector3' 0 0 (-1),
-- 'Vector3' 0 1 0), i.e. looking down the Z axis with the Y axis pointing
-- upwards.

orientation :: StateVar (Vector3 ALfloat, Vector3 ALfloat)
orientation = makeListenerVar GetOrientation 6 (peek6 Vector3) listenerVector6

--------------------------------------------------------------------------------

listenerf :: GetPName -> ALfloat -> IO ()
listenerf = alListenerf . marshalGetPName

foreign import CALLCONV unsafe "alListenerf"
   alListenerf :: ALenum -> ALfloat -> IO ()

--------------------------------------------------------------------------------

listener3f :: Storable a => GetPName -> a -> IO ()
listener3f n x = with x $ listenerfv n

listenerVector6 :: GetPName -> (Vector3 ALfloat, Vector3 ALfloat) -> IO ()
listenerVector6 n (x, y) = withArray [x, y] $ listenerfv n

listenerfv :: GetPName -> Ptr a -> IO ()
listenerfv = alListenerfv . marshalGetPName

foreign import CALLCONV unsafe "alListenerfv"
   alListenerfv :: ALenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

getListenerfv :: GetPName -> Ptr ALfloat -> IO ()
getListenerfv = alGetListenerfv . marshalGetPName

foreign import CALLCONV unsafe "alGetListenerfv"
   alGetListenerfv :: ALenum -> Ptr ALfloat -> IO ()

--------------------------------------------------------------------------------

makeListenerVar :: GetPName -> Int -> (Ptr ALfloat -> IO a)
                -> (GetPName -> a -> IO ()) -> StateVar a
makeListenerVar pname size reader writer =
   makeStateVar
      (allocaArray size $ \buf -> do
          getListenerfv pname buf
          reader buf)
      (writer pname)
