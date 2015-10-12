--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.Attenuation
-- Copyright   :  (c) Sven Panne 2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.4 (Attenuation By Distance) of the
-- OpenAL Specification and Reference (version 1.1).
-- 
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.Attenuation (
   -- * Introduction
   -- $Introduction

   -- * Handling the Distance Model
   DistanceModel(..), distanceModel

   -- * Evaluation of Gain\/Attenuation Related State
   -- $EvaluationOfGainAttenuationRelatedState

   -- * No Culling By Distance
   -- $NoCullingByDistance
) where

import Foreign.Marshal.Alloc ( alloca )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )
import Sound.OpenAL.AL.BasicTypes ( ALint, ALenum )
import Sound.OpenAL.AL.PeekPoke ( peek1 )
import Sound.OpenAL.AL.QueryUtils (
   GetPName(GetDistanceModel), marshalGetPName )
import Sound.OpenAL.Constants (
   al_NONE, al_INVERSE_DISTANCE, al_INVERSE_DISTANCE_CLAMPED,
   al_LINEAR_DISTANCE, al_LINEAR_DISTANCE_CLAMPED, al_EXPONENT_DISTANCE,
   al_EXPONENT_DISTANCE_CLAMPED )

#ifdef __HADDOCK__
import Sound.OpenAL.AL.Source (
   sourceGain, gainBounds, coneAngles, coneOuterGain, referenceDistance,
   rolloffFactor, maxDistance )
#endif

--------------------------------------------------------------------------------
-- $Introduction
-- Samples usually use the entire dynamic range of the chosen format\/encoding,
-- independent of their real world intensity. In other words, a jet engine and a
-- clockwork both will have samples with full amplitude. The application will
-- then have to adjust source gain accordingly to account for relative
-- differences.
-- 
-- Source gain is then attenuated by distance. The effective attenuation of a
-- source depends on many factors, among which distance attenuation and source
-- and listener gain are only some of the contributing factors. Even if the
-- source and listener gain exceed 1 (amplification beyond the guaranteed
-- dynamic range), distance and other attenuation might ultimately limit the
-- overall gain to a value below 1.

--------------------------------------------------------------------------------
-- $EvaluationOfGainAttenuationRelatedState
-- While amplification\/attenuation commute (multiplication of scaling factors),
-- clamping operations do not. The order in which various gain related
-- operations are applied is:
-- 
-- 1. Distance attenuation is calculated first, including minimum
-- ('referenceDistance') and maximum ('maxDistance') thresholds.
-- 
-- 2. The result is then multiplied by source gain.
-- 
-- 3. If the source is directional (the inner cone angle is less than the outer
-- cone angle, see 'coneAngles'), an angle-dependent attenuation is calculated
-- depending on 'coneOuterGain', and multiplied with the distance-dependent
-- attenuation. The resulting attenuation factor for the given angle and
-- distance between listener and source is multiplied with 'sourceGain'.
-- 
-- 4. The effective gain computed this way is compared against 'gainBounds'.
-- 
-- 5. The result is guaranteed to be clamped to 'gainBounds', and subsequently
-- multiplied by listener gain which serves as an overall volume control.
-- 
-- The implementation is free to clamp listener gain if necessary due to
-- hardware or implementation constraints.

--------------------------------------------------------------------------------
-- $NoCullingByDistance
-- With the DS3D compatible inverse clamped distance model, OpenAL provides a
-- per-source 'maxDistance' attribute that can be used to define a distance
-- beyond which the source will not be further attenuated by distance. The DS3D
-- distance attenuation model and its clamping of volume is also extended by a
-- mechanism to cull (mute) sources from processing, based on distance. However,
-- the OpenAL does not support culling a source from processing based on a
-- distance threshold.
-- 
-- At this time OpenAL is not meant to support culling at all. Culling based on
-- distance, or bounding volumes, or other criteria, is best left to the
-- application. For example, the application might employ sophisticated
-- techniques to determine whether sources are audible that are beyond the scope
-- of OpenAL. In particular, rule based culling inevitably introduces acoustic
-- artifacts. E.g. if the listener-source distance is nearly equal to the culling
-- threshold distance, but varies above and below, there will be popping
-- artifacts in the absence of hysteresis.

--------------------------------------------------------------------------------

-- | OpenAL currently supports six modes of operation with respect to distance
-- attenuation, including one that is similar to the IASIG I3DL2 model. The
-- application chooses one of these models (or chooses to disable
-- distance-dependent attenuation) on a per-context basis.
--
-- The distance used in the formulas for the \"clamped\" modes below is clamped
-- to be in the range between 'referenceDistance' and 'maxDistance':
--
-- /clamped distance/ =
--    max('referenceDistance', min(/distance/, 'maxDistance'))
--
-- The linear models are not physically realistic, but do allow full attenuation
-- of a source beyond a specified distance. The OpenAL implementation is still
-- free to apply any range clamping as necessary.
-- 
-- With all the distance models, if the formula can not be evaluated then the
-- source will not be attenuated. For example, if a linear model is being used
-- with 'referenceDistance' equal to 'maxDistance', then the gain equation will
-- have a divide-by-zero error in it. In this case, there is no attenuation for
-- that source.

data DistanceModel =
     NoAttenuation
   -- ^ Bypass all distance attenuation calculation for all sources. The
   --   implementation is expected to optimize this situation.
   | InverseDistance
   -- ^ Inverse distance rolloff model, which is equivalent to the IASIG I3DL2
   --   model with the exception that 'referenceDistance' does not imply any
   --   clamping.
   --
   --   /gain/ = 'referenceDistance' \/ ('referenceDistance' +
   --             'rolloffFactor' \* (/distance/ - 'referenceDistance'))
   --
   --   The 'referenceDistance' parameter used here is a per-source attribute
   --   which is the distance at which the listener will experience gain
   --   (unless the implementation had to clamp effective gain to the available
   --   dynamic range). 'rolloffFactor' is per-source parameter the application
   --   can use to increase or decrease the range of a source by decreasing or
   --   increasing the attenuation, respectively. The default value is 1. The
   --   implementation is free to optimize for a 'rolloffFactor' value of 0,
   --   which indicates that the application does not wish any distance
   --   attenuation on the respective source.
   | InverseDistanceClamped
   -- ^ Inverse Distance clamped model, which is essentially the inverse
   --   distance rolloff model, extended to guarantee that for distances below
   --   'referenceDistance', gain is clamped. This mode is equivalent to the
   --   IASIG I3DL2 distance model.
   | LinearDistance
   -- ^ Linear distance rolloff model, modeling a linear dropoff in gain as
   -- distance increases between the source and listener.
   --
   --   /gain/ = (1 - 'rolloffFactor' \* (/distance/ - 'referenceDistance') \/
   --            ('maxDistance' - 'referenceDistance'))
   | LinearDistanceClamped
   -- ^ Linear Distance clamped model, which is the linear model, extended to
   --   guarantee that for distances below 'referenceDistance', gain is clamped.
   | ExponentDistance
   -- ^ Exponential distance rolloff model, modeling an exponential dropoff in
   --   gain as distance increases between the source and listener.
   --
   --   /gain/ = (/distance/ \/ 'referenceDistance') \*\* (- 'rolloffFactor')
   | ExponentDistanceClamped
   -- ^ Exponential Distance clamped model, which is the exponential model,
   --   extended to guarantee that for distances below 'referenceDistance',
   --   gain is clamped.
   deriving ( Eq, Ord, Show )

marshalDistanceModel :: DistanceModel -> ALenum
marshalDistanceModel x = case x of
   NoAttenuation -> al_NONE
   InverseDistance -> al_INVERSE_DISTANCE
   InverseDistanceClamped -> al_INVERSE_DISTANCE_CLAMPED
   LinearDistance -> al_LINEAR_DISTANCE
   LinearDistanceClamped -> al_LINEAR_DISTANCE_CLAMPED
   ExponentDistance -> al_EXPONENT_DISTANCE
   ExponentDistanceClamped -> al_EXPONENT_DISTANCE_CLAMPED

unmarshalDistanceModel :: ALenum -> DistanceModel
unmarshalDistanceModel x
   | x == al_NONE = NoAttenuation
   | x == al_INVERSE_DISTANCE = InverseDistance
   | x == al_INVERSE_DISTANCE_CLAMPED = InverseDistanceClamped
   | x == al_LINEAR_DISTANCE = LinearDistance
   | x == al_LINEAR_DISTANCE_CLAMPED = LinearDistanceClamped
   | x == al_EXPONENT_DISTANCE = ExponentDistance
   | x == al_EXPONENT_DISTANCE_CLAMPED = ExponentDistanceClamped
   | otherwise = error ("unmarshalDistanceModel: illegal value " ++ show x)

-- | Contains the current per-context distance model.

distanceModel :: StateVar DistanceModel
distanceModel =
   makeStateVar
      (alloca $ \buf -> do
          alGetIntegerv (marshalGetPName GetDistanceModel) buf
          peek1 (unmarshalDistanceModel . fromIntegral) buf)
      (alDistanceModel . marshalDistanceModel)

foreign import CALLCONV unsafe "alGetIntegerv"
   alGetIntegerv :: ALenum -> Ptr ALint -> IO ()

foreign import CALLCONV unsafe "alDistanceModel"
   alDistanceModel :: ALenum -> IO ()
