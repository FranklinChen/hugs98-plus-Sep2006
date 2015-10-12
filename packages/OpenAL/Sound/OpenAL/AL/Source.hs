--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.Source
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to sections 4.1 (Basic Listener and Source
-- Attributes) and 4.3 (Source Objects) of the OpenAL Specification and
-- Reference (version 1.1).
--
-- Sources specify attributes like position, velocity, and a buffer with sample
-- data. By controlling a source\'s attributes the application can modify and
-- parameterize the static sample data provided by the buffer referenced by the
-- source. Sources define a localized sound, and encapsulate a set of attributes
-- applied to a sound at its origin, i.e. in the very first stage of the
-- processing on the way to the listener. Source related effects have to be
-- applied before listener related effects unless the output is invariant to any
-- collapse or reversal of order. OpenAL also provides additional functions to
-- manipulate and query the execution state of sources: the current playing
-- status of a source, including access to the current sampling position within
-- the associated buffer.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.Source (
   -- * The Source Type
   Source,

   -- * Source Attributes

   -- ** Basic Source Attributes
   sourcePosition, sourceVelocity, sourceGain,

   -- ** Source Positioning
   SourceRelative(..), sourceRelative,

   -- ** Source Type
   SourceType(..), sourceType,

   -- ** Buffer Looping
   LoopingMode(..), loopingMode,

   -- ** Current Buffer
   buffer,

   -- ** Queue State Queries
   buffersQueued, buffersProcessed,

   -- ** Bounds on Gain
   gainBounds,

   -- ** Distance Model Attributes
   referenceDistance, rolloffFactor, maxDistance,

   -- ** Frequency Shift by Pitch
   pitch,

   -- ** Direction and Cone
   -- $DirectionAndCone
   direction, coneAngles, coneOuterGain,

   -- ** Offset
   secOffset, sampleOffset, byteOffset,

   -- * Queuing Buffers with a Source
   -- $QueuingBuffersWithASource
   queueBuffers, unqueueBuffers,

   -- * Managing Source Execution
   -- $ManagingSourceExecution
   SourceState(..), sourceState, play, pause, stop, rewind
) where

import Control.Monad ( liftM2 )
import Foreign.Marshal.Array ( withArrayLen, peekArray, allocaArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL.GL.BufferObjects ( ObjectName(..) )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Vector3(..) )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Vertex3(..) )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), HasSetter(($=)), makeStateVar, StateVar, GettableStateVar,
   makeGettableStateVar )
import Sound.OpenAL.AL.ALboolean ( marshalALboolean, unmarshalALboolean )
import Sound.OpenAL.AL.BasicTypes
import Sound.OpenAL.AL.Buffer ( Buffer )
import Sound.OpenAL.AL.BufferInternal ( marshalBuffer, unmarshalBuffer )
import Sound.OpenAL.AL.Listener ( Gain )
import Sound.OpenAL.AL.PeekPoke ( peek1, poke1, peek3 )
import Sound.OpenAL.AL.SourceState ( SourceState(..), unmarshalSourceState )
import Sound.OpenAL.AL.QueryUtils (
   GetPName(GetPosition,GetVelocity,GetGain,GetSourceRelative,GetSourceType,
            GetLooping,GetBuffer,GetBuffersQueued,GetBuffersProcessed,
            GetMinGain,GetMaxGain,GetReferenceDistance,GetRolloffFactor,
            GetMaxDistance,GetPitch,GetDirection,GetConeInnerAngle,
            GetConeOuterAngle,GetConeOuterGain,GetSecOffset,GetSampleOffset,
            GetByteOffset,GetSourceState),
   marshalGetPName )
import Sound.OpenAL.Constants ( al_UNDETERMINED, al_STATIC, al_STREAMING )

#ifdef __HADDOCK__
import Sound.OpenAL.AL.Listener ( listenerGain )
import Sound.OpenAL.AL.Errors ( ALErrorCategory(..) )
#endif

--------------------------------------------------------------------------------

-- | The abstract buffer type.

newtype Source = Source ALuint
   deriving ( Eq, Ord, Show )

instance Storable Source where
   sizeOf    ~(Source b) = sizeOf b
   alignment ~(Source b) = alignment b
   peek                  = peek1 Source . castPtr
   poke ptr   (Source b) = poke1 (castPtr ptr) b

instance ObjectName Source where
   genObjectNames n =
      allocaArray n $ \buf -> do
         alGenSources (fromIntegral n) buf
         peekArray n buf

   deleteObjectNames = withArraySizei alDeleteSources

   isObjectName = fmap unmarshalALboolean . alIsSource

foreign import CALLCONV unsafe "alGenSources"
   alGenSources :: ALsizei -> Ptr Source -> IO ()

foreign import CALLCONV unsafe "alDeleteSources"
   alDeleteSources :: ALsizei -> Ptr Source -> IO ()

foreign import CALLCONV unsafe "alIsSource"
   alIsSource :: Source -> IO ALboolean

--------------------------------------------------------------------------------

-- | 'sourcePosition' contains the current location of the source in the world
-- coordinate system. Any 3-tuple of valid float values is allowed.
-- Implementation behavior on encountering NaN and infinity is not defined. The
-- initial position is ('Vertex3' 0 0 0).

sourcePosition :: Source -> StateVar (Vertex3 ALfloat)
sourcePosition = makeSourceStateVar dictVertex3ALfloat GetPosition

-- | 'sourceVelocity' contains current velocity (speed and direction) of the
-- source in the world coordinate system. Any 3-tuple of valid float values is
-- allowed, and the initial velocity is ('Vector3' 0 0 0). 'sourceVelocity' does
-- not affect 'sourcePosition'. OpenAL does not calculate the velocity from
-- subsequent position updates, nor does it adjust the position over time based
-- on the specified velocity. Any such calculation is left to the application.
-- For the purposes of sound processing, position and velocity are independent
-- parameters affecting different aspects of the sounds.
--
-- 'sourceVelocity' is taken into account by the driver to synthesize the
-- Doppler effect perceived by the listener for each source, based on the
-- velocity of both source and listener, and the Doppler related parameters.

sourceVelocity :: Source -> StateVar (Vector3 ALfloat)
sourceVelocity = makeSourceStateVar dictVector3ALfloat GetVelocity

-- | 'sourceGain' contains a scalar amplitude multiplier for the given source.
-- The initial value 1 means that the sound is unattenuated. A 'sourceGain'
-- value of 0.5 is equivalent to an attenuation of 6dB. The value zero equals
-- silence (no output). Driver implementations are free to optimize this case
-- and skip mixing and processing stages where applicable. The implementation is
-- in charge of ensuring artifact-free (click-free) changes of gain values and
-- is free to defer actual modification of the sound samples, within the limits
-- of acceptable latencies.
--
-- A 'sourceGain' larger than 1 (amplification) is permitted. However, the
-- implementation is free to clamp the total gain (effective gain per source
-- times listener gain) to 1 to prevent overflow.

sourceGain :: Source -> StateVar Gain
sourceGain = makeSourceStateVar dictALfloat GetGain

--------------------------------------------------------------------------------

-- | The entity to which the source attributes 'sourcePosition',
-- 'sourceVelocity' and 'direction' are to be interpreted.

data SourceRelative =
     World
   | Listener
   deriving ( Eq, Ord, Show )

marshalSourceRelative :: SourceRelative -> Bool
marshalSourceRelative = (== Listener)

unmarshalSourceRelative :: Bool -> SourceRelative
unmarshalSourceRelative x = if x then Listener else World

-- | If 'sourceRelative' contains 'Listener', it indicates indicates that the
-- values specified by 'sourcePosition', 'sourceVelocity' and 'direction' are to
-- be interpreted relative to the listener position. The initial value is
-- 'World', indicating that those source attributes are to be interpreted
-- relative to the world, i.e. they are considered absolute.

sourceRelative :: Source -> StateVar SourceRelative
sourceRelative = makeSourceStateVar dictSourceRelative GetSourceRelative

--------------------------------------------------------------------------------

-- | When first created, a source will be in the 'Undetermined' state. If a
-- buffer is then attached using 'buffer', then the source will enter the
-- 'Static' state. If the first buffer attached to a source is attached using
-- 'queueBuffers', then the source will enter the 'Streaming' state. A source of
-- either state will be reset to state 'Undetermined' by setting its 'buffer' to
-- 'Nothing', and attaching any buffer to a streaming source will change the
-- state to 'Static'. Attempting to queue a buffer on a static source will
-- result in an 'ALInvalidOperation' error.

data SourceType =
     Undetermined
   | Static
   | Streaming
   deriving ( Eq, Ord, Show )

unmarshalSourceType :: ALint -> SourceType
unmarshalSourceType x
   | x == al_UNDETERMINED = Undetermined
   | x == al_STATIC = Static
   | x == al_STREAMING = Streaming
   | otherwise = error ("unmarshalSourceType: illegal value " ++ show x)

-- | 'sourceType' indicates whether a source is ready to queue buffers, ready to
-- use a static buffer, or is in an undetermined state where it can be used for
-- either streaming or static playback.

sourceType :: Source -> GettableStateVar SourceType
sourceType = makeSourceGettableStateVar dictSourceType GetSourceType

--------------------------------------------------------------------------------

-- | Specifies what should happen when the end of a buffer queue is reached.

data LoopingMode =
     OneShot
   | Looping
   deriving ( Eq, Ord, Show )

marshalLoopingMode :: LoopingMode -> Bool
marshalLoopingMode = (== Looping)

unmarshalLoopingMode :: Bool -> LoopingMode
unmarshalLoopingMode x = if x then Looping else OneShot

-- | If 'loopingMode' contains 'Looping', it indicates that the source will not
-- be in the 'Stopped' state once it reaches the end of last buffer in the
-- buffer queue. Instead, the source will immediately promote to 'Initial' and
-- 'Playing'.  The initial value is 'OneShot'. 'loopingMode' can be changed on a
-- source in any execution state. In particular, it can be changed on a
-- 'Playing' source.

loopingMode :: Source -> StateVar LoopingMode
loopingMode = makeSourceStateVar dictLoopingMode GetLooping

--------------------------------------------------------------------------------

-- | 'buffer' contains the current buffer object. Setting 'buffer' to 'Just' a
-- buffer object makes it the head entry in the source\'s queue. Setting
-- 'buffer'for a source in the 'Stopped' or 'Initial' state empties the entire
-- queue, then appends the one buffer specified (or none at all if 'Nothing'
-- was specified).
-- 
-- For a source in the 'Playing' or 'Paused' state, setting 'buffer' will result
-- in the 'ALInvalidOperation' error state being set. 'buffer' can be applied only
-- to sources in the 'Initial' and 'Stopped' states. Specifying an invalid
-- buffer name (either because the buffer name doesn\'t exist or because that
-- buffer can\'t be attached to the specified source) will result in an
-- 'ALInvalidValue' error while specifying an invalid source name results in an
-- 'ALInvalidName' error. Setting 'buffer' to 'Nothing' is a legal way to release
-- the current buffer queue on a source in the 'Initial' or 'Stopped' state,
-- whether the source has just one entry (current buffer) or more. Setting
-- 'buffer' to 'Nothing' still causes an 'ALInvalidOperation' for any source in
-- the 'Playing' or 'Paused' state, consequently it cannot be used to mute or
-- stop a source. The initial value is 'Nothing'.

buffer :: Source -> StateVar (Maybe Buffer)
buffer = makeSourceStateVar dictMaybeBuffer GetBuffer

--------------------------------------------------------------------------------

-- | 'buffersQueued' contains the number of buffers in the queue of a given
-- source. This includes those not yet played, the one currently playing, and
-- the ones that have been played already. It will contain 0 if 'buffer' has
-- been set to 'Nothing'.

buffersQueued :: Source -> GettableStateVar ALint
buffersQueued = makeSourceGettableStateVar dictALint GetBuffersQueued

-- | 'buffersProcessed' contains the number of buffers that have been played
-- by a given source.  Indirectly, this gives the index of the buffer currently
-- playing. It can be used to determine how much slots are needed for unqueuing
-- them. On a source in the 'Stopped' state, all buffers are processed. On a
-- source in the 'Initial' state, no buffers are processed, all buffers are
-- pending. It will contain 0 if 'buffer' has been set to 'Nothing'.

buffersProcessed :: Source -> GettableStateVar ALint
buffersProcessed = makeSourceGettableStateVar dictALint GetBuffersProcessed

--------------------------------------------------------------------------------

-- | 'gainBounds' contains two scalar amplitude thresholds between 0 and 1
-- (included): The minimum guaranteed gain for this source and the maximum gain
-- permitted, with initial values 0 and 1, respectively At the end of the
-- processing of various attenuation factors such as distance based attenuation
-- and 'sourceGain', the effective gain calculated is compared to these values:
--
-- If the effective gain is lower than the minimum gain, the minimum gain is
-- applied. This happens before the 'listenerGain' is applied. If a zero minimum
-- gain is set, then the effective gain will not be corrected.
--
-- If the effective gain is higher than the maximum gain, the maximum gain is
-- applied. This happens before the 'listenerGain' is applied. If the
-- 'listenerGain' times the maximum gain still exceeds the maximum gain the
-- implementation can handle, the implementation is free to clamp. If a zero
-- maximum gain is set, then the source is effectively muted. The implementation
-- is free to optimize for this situation, but no optimization is required or
-- recommended as setting 'sourceGain' to zero is the proper way to mute a
-- source.

gainBounds :: Source -> StateVar (Gain, Gain)
gainBounds source =
   pairStateVars
      (makeSourceStateVar dictALfloat GetMinGain source)
      (makeSourceStateVar dictALfloat GetMaxGain source)

--------------------------------------------------------------------------------

-- | 'referenceDistance' is used for distance attenuation calculations based on
-- inverse distance with rolloff. Depending on the distance model it will also
-- act as a distance threshold below which gain is clamped. See
-- "Sound.OpenAL.AL.Attenuation" for details. The initial value is 1.

referenceDistance :: Source -> StateVar ALfloat
referenceDistance = makeSourceStateVar dictALfloat GetReferenceDistance

-- | 'rolloffFactor' is used for distance attenuation calculations based on
-- inverse distance with rolloff. For distances smaller than 'maxDistance' (and,
-- depending on the distance model, larger than 'referenceDistance'), this will
-- scale the distance attenuation over the applicable range. See
-- "Sound.OpenAL.AL.Attenuation" for details how the attenuation is computed as
-- a function of the distance. The initial value is 1.
-- 
-- In particular, 'rolloffFactor' can be set to zero for those sources that are
-- supposed to be exempt from distance attenuation. The implementation is
-- encouraged to optimize this case, bypassing distance attenuation calculation
-- entirely on a persource basis.

rolloffFactor :: Source -> StateVar ALfloat
rolloffFactor = makeSourceStateVar dictALfloat GetRolloffFactor

-- | 'maxDistance' is used for distance attenuation calculations based on
-- inverse distance with rolloff, if the inverse clamped distance model is
-- used. In this case, distances greater than 'maxDistance' will be clamped to
-- 'maxDistance'.  'maxDistance' based clamping is applied before minimum gain
-- clamping (see 'gainBounds'), so if the effective gain at 'maxDistance' is
-- larger than the minimum gain, the minimum gain will have no effect. No
-- culling is supported. The initial value is the largest representable
-- 'ALfloat'.

maxDistance :: Source -> StateVar ALfloat
maxDistance = makeSourceStateVar dictALfloat GetMaxDistance

--------------------------------------------------------------------------------

-- | 'pitch' contains the desired pitch shift, where 1 (the initial value)
-- equals identity. Each reduction by 50 percent equals a pitch shift of -12
-- semitones (one octave reduction). Each doubling equals a pitch shift of 12
-- semitones (one octave increase). Zero is not a legal value. Implementations
-- may clamp the actual output pitch range to any values subject to the
-- implementation's own limits.

pitch :: Source -> StateVar ALfloat
pitch = makeSourceStateVar dictALfloat GetPitch

--------------------------------------------------------------------------------
-- $DirectionAndCone
-- Each source can be directional, depending on the settings for 'coneAngles'.
-- There are three zones defined: the inner cone, the outside zone, and the
-- transitional zone in between. The angle-dependent gain for a directional
-- source is constant inside the inner cone, and changes over the transitional
-- zone to the value specified outside the outer cone. 'sourceGain' is applied
-- for the inner cone, with an application selectable 'coneOuterGain' factor to
-- define the gain in the outer zone. In the transitional zone
-- implementation-dependent interpolation between 'sourceGain' and 'sourceGain'
-- times 'coneOuterGain' is applied.

--------------------------------------------------------------------------------

-- | If 'direction' does not contain the zero vector ('Vector3' 0 0 0), the
-- source is directional. The sound emission is presumed to be symmetric around
-- the direction vector (cylinder symmetry). Sources are not oriented in full 3
-- degrees of freedom, only two angles are effectively needed.
--
-- The zero vector is the initial value, indicating that a source is not
-- directional. Specifying a non-zero vector will make the source directional.
-- Specifying a zero vector for a directional source will effectively mark it as
-- nondirectional.

direction :: Source -> StateVar (Vector3 ALfloat)
direction = makeSourceStateVar dictVector3ALfloat GetDirection

-- | 'coneAngles' contains the inner and outer angles of the sound cone, in
-- degrees. The default of 360 for the inner cone angle means that it covers the
-- entire world, which is equivalent to an omni-directional source. The default
-- of 360 for the outer cone angle means that it covers the entire world. If the
-- inner angle is also 360, then the zone for angle-dependent attenuation is
-- zero.

coneAngles :: Source -> StateVar (ALfloat, ALfloat)
coneAngles source =
   pairStateVars
      (makeSourceStateVar dictALfloat GetConeInnerAngle source)
      (makeSourceStateVar dictALfloat GetConeOuterAngle source)

-- | 'coneOuterGain' contains the factor with which 'sourceGain' is multiplied
-- to determine the effective gain outside the cone defined by the outer angle.
-- The effective gain applied outside the outer cone is 'sourceGain' times
-- 'coneOuterGain'. Changing 'sourceGain' affects all directions, i.e. the
-- source is attenuated in all directions, for any position of the listener. The
-- application has to change 'coneOuterGain' as well if a different behavior is
-- desired.

coneOuterGain :: Source -> StateVar Gain
coneOuterGain = makeSourceStateVar dictALfloat GetConeOuterGain

--------------------------------------------------------------------------------

-- | 'secOffset' contains the playback position, expressed in seconds (the value
-- will loop back to zero for looping sources).
--
-- When setting 'secOffset' on a source which is already playing, the playback
-- will jump to the new offset unless the new offset is out of range, in which
-- case an 'ALInvalidValue' error is set. If the source is not playing, then the
-- offset will be applied on the next 'play' call.
--
-- The position is relative to the beginning of all the queued buffers for the
-- source, and any queued buffers traversed by a set call will be marked as
-- processed.
--
-- This value is based on byte position, so a pitch-shifted source will have an
-- exaggerated playback speed. For example, you can be 0.5 seconds into a buffer
-- having taken only 0.25 seconds to get there if the pitch is set to 2.

secOffset :: Source -> StateVar ALfloat
secOffset = makeSourceStateVar dictALfloat GetSecOffset

-- | 'sampleOffset' contains the playback position, expressed in samples (the
-- value will loop back to zero for looping sources). For a compressed format,
-- this value will represent an exact offset within the uncompressed data.
-- 
-- When setting 'sampleOffset' on a source which is already playing, the
-- playback will jump to the new offset unless the new offset is out of range,
-- in which case an 'ALInvalidValue' error is set. If the source is not playing,
-- then the offset will be applied on the next 'play' call. A 'stop', 'rewind',
-- or a second 'play' call will reset the offset to the beginning of the buffer.
-- 
-- The position is relative to the beginning of all the queued buffers for the
-- source, and any queued buffers traversed by a set call will be marked as
-- processed.

sampleOffset :: Source -> StateVar ALint
sampleOffset = makeSourceStateVar dictALint GetSampleOffset

-- | 'byteOffset' contains the playback position, expressed in bytes (the value
-- will loop back to zero for looping sources). For a compressed format, this
-- value may represent an approximate offset within the compressed data buffer.
-- 
-- When setting 'byteOffset' on a source which is already playing, the playback
-- will jump to the new offset unless the new offset is out of range, in which
-- case an 'ALInvalidValue' error is set. If the source is not playing, then the
-- offset will be applied on the next 'play' call. A 'stop', 'rewind', or a
-- second 'play' call will reset the offset to the beginning of the buffer.
-- 
-- The position is relative to the beginning of all the queued buffers for the
-- source, and any queued buffers traversed by a set call will be marked as
-- processed.

byteOffset :: Source -> StateVar ALint
byteOffset = makeSourceStateVar dictALint GetByteOffset

--------------------------------------------------------------------------------

pairStateVars :: StateVar a -> StateVar b -> StateVar (a,b)
pairStateVars var1 var2 =
   makeStateVar
      (liftM2 (,) (get var1) (get var2))
      (\(val1,val2) -> do var1 $= val1; var2 $= val2)

data Dictionary a b c = Dictionary {
   alGetter  :: Source -> ALenum -> Ptr b -> IO (),
   alSetter  :: Source -> ALenum -> Ptr b -> IO (),
   size      :: Int,
   peekSize  :: Ptr b -> IO a,
   marshal   :: a -> c }

dictALint :: Dictionary ALint ALint ALint
dictALint = Dictionary {
   alGetter  = alGetSourceiv,
   alSetter  = alSourceiv,
   size      = 1,
   peekSize  = peek1 id,
   marshal   = id }

dictALfloat :: Dictionary ALfloat ALfloat ALfloat
dictALfloat = Dictionary {
   alGetter  = alGetSourcefv,
   alSetter  = alSourcefv,
   size      = 1,
   peekSize  = peek1 id,
   marshal   = id }

dictSourceRelative :: Dictionary SourceRelative ALint ALint
dictSourceRelative = Dictionary {
   alGetter  = alGetSourceiv,
   alSetter  = alSourceiv,
   size      = 1,
   peekSize  = peek1 (unmarshalSourceRelative . unmarshalALboolean . fromIntegral),
   marshal   = fromIntegral . marshalALboolean . marshalSourceRelative }

dictSourceType :: Dictionary SourceType ALint ALint
dictSourceType = Dictionary {
   alGetter  = alGetSourceiv,
   alSetter  = undefined,
   size      = 1,
   peekSize  = peek1 unmarshalSourceType,
   marshal   = undefined }

dictLoopingMode :: Dictionary LoopingMode ALint ALint
dictLoopingMode = Dictionary {
   alGetter  = alGetSourceiv,
   alSetter  = alSourceiv,
   size      = 1,
   peekSize  = peek1 (unmarshalLoopingMode . unmarshalALboolean . fromIntegral),
   marshal   = fromIntegral . marshalALboolean . marshalLoopingMode }

dictSourceState :: Dictionary SourceState ALint ALint
dictSourceState = Dictionary {
   alGetter  = alGetSourceiv,
   alSetter  = undefined,
   size      = 1,
   peekSize  = peek1 unmarshalSourceState,
   marshal   = undefined }

dictVertex3ALfloat :: Dictionary (Vertex3 ALfloat) ALfloat (Vertex3 ALfloat)
dictVertex3ALfloat = Dictionary {
   alGetter  = alGetSourcefv,
   alSetter  = alSourcefv,
   size      = 3,
   peekSize  = peek3 Vertex3,
   marshal   = id }

dictVector3ALfloat :: Dictionary (Vector3 ALfloat) ALfloat (Vector3 ALfloat)
dictVector3ALfloat = Dictionary {
   alGetter  = alGetSourcefv,
   alSetter  = alSourcefv,
   size      = 3,
   peekSize  = peek3 Vector3,
   marshal   = id }

dictMaybeBuffer :: Dictionary (Maybe Buffer) ALint ALint
dictMaybeBuffer = Dictionary {
   alGetter  = alGetSourceiv,
   alSetter  = alSourceiv,
   size      = 1,
   peekSize  = peek1 (unmarshalBuffer . fromIntegral),
   marshal   = fromIntegral . marshalBuffer }

makeGetter :: Storable b => Dictionary a b c -> GetPName -> Source -> IO a
makeGetter dict name source =
   allocaArray (size dict) $ \buf -> do
      alGetter dict source (marshalGetPName name) buf
      peekSize dict buf

makeSetter :: Storable c => Dictionary a b c -> GetPName -> Source -> a -> IO ()
makeSetter dict name source value =
   with (marshal dict value) $
      alSetter dict source (marshalGetPName name) . castPtr

makeSourceStateVar ::
   (Storable b, Storable c) =>
   Dictionary a b c -> GetPName -> Source -> StateVar a
makeSourceStateVar dict name source =
   makeStateVar
      (makeGetter dict name source)
      (makeSetter dict name source)

makeSourceGettableStateVar ::
   (Storable b, Storable c) =>
   Dictionary a b c -> GetPName -> Source -> GettableStateVar a
makeSourceGettableStateVar dict name source =
   makeGettableStateVar
      (makeGetter dict name source)

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "alGetSourcefv"
   alGetSourcefv :: Source -> ALenum -> Ptr ALfloat -> IO ()

foreign import CALLCONV unsafe "alSourcefv"
   alSourcefv :: Source -> ALenum -> Ptr ALfloat -> IO ()

foreign import CALLCONV unsafe "alGetSourceiv"
   alGetSourceiv :: Source ->  ALenum -> Ptr ALint -> IO ()

-- Note: Older OpenAL implementations have no alSourceiv, so we emulate it here.
alSourceiv :: Source -> ALenum -> Ptr ALint -> IO ()
alSourceiv source n buf =  peek buf >>= alSourcei source n

foreign import CALLCONV unsafe "alSourcei"
   alSourcei :: Source -> ALenum -> ALint -> IO ()

--------------------------------------------------------------------------------
-- $QueuingBuffersWithASource
-- OpenAL does not specify a built-in streaming mechanism. There is no mechanism
-- to stream data into a buffer object. Instead, the API has a more flexible and
-- versatile mechanism to queue buffers for sources. There are many ways to use
-- this feature, with streaming being only one of them.
--
-- Streaming is replaced by queuing static buffers. This effectively moves any
-- multi-buffer caching into the application and allows the application to
-- select how many buffers it wants to use, the size of the buffers, and whether
-- these are re-used in cycle, pooled, or thrown away.
--
-- Looping (over a finite number of repetitions) can be implemented by
-- explicitly repeating buffers in the queue. Infinite loops can (theoretically)
-- be accomplished by sufficiently large repetition counters. If only a single
-- buffer is supposed to be repeated infinitely, using the respective source
-- attribute 'loopingMode' is recommended.
--
-- Loop Points for restricted looping inside a buffer can in many cases be
-- replaced by splitting the sample into several buffers and queuing the sample
-- fragments (including repetitions) accordingly.
--
-- Buffers can be queued, unqueued after they have been used, and either be
-- deleted, or refilled and queued again. Splitting large samples over several
-- buffers maintained in a queue has distinct advantages over approaches that
-- require explicit management of samples and sample indices.

--------------------------------------------------------------------------------

-- | The application can queue up one or multiple buffer names using
-- 'queueBuffers'. The buffers will be queued in the sequence in which they
-- appear in the list.
--
-- This command is legal on a source in any playback state (to allow for
-- streaming, queuing has to be possible on a 'Playing' source).
--
-- All buffers in a queue must have the same format and attributes. An attempt
-- to mix formats or other buffer attributes will result in a failure and an
-- 'ALInvalidValue' error will be thrown. If the queue operation fails, the source
-- queue will remain unchanged (even if some of the buffers could have been
-- queued).

queueBuffers :: Source -> [Buffer] -> IO ()
queueBuffers = withArraySizei . alSourceQueueBuffers

withArraySizei :: Storable a => (ALsizei -> Ptr a -> IO ()) -> [a] -> IO ()
withArraySizei f xs = withArrayLen xs $ f . fromIntegral

foreign import CALLCONV unsafe "alSourceQueueBuffers"
   alSourceQueueBuffers :: Source -> ALsizei -> Ptr Buffer -> IO ()

--------------------------------------------------------------------------------

-- | Once a queue entry for a buffer has been appended to a queue and is pending
-- processing, it should not be changed. Removal of a given queue entry is not
-- possible unless either the source is stopped (in which case then entire queue
-- is considered processed), or if the queue entry has already been processed
-- ('Playing' or 'Paused' source). A playing source will enter the 'Stopped'
-- state if it completes playback of the last buffer in its queue (the same
-- behavior as when a single buffer has been attached to a source and has
-- finished playback).
--
-- The 'unqueueBuffers' command removes a number of buffers entries that have
-- finished processing, in the order of appearance, from the queue. The
-- operation will fail with an 'ALInvalidValue' error if more buffers are
-- requested than available, leaving the destination arguments unchanged.

unqueueBuffers :: Source -> [Buffer] -> IO ()
unqueueBuffers = withArraySizei . alSourceUnqueueBuffers

foreign import CALLCONV unsafe "alSourceUnqueueBuffers"
   alSourceUnqueueBuffers :: Source -> ALsizei -> Ptr Buffer -> IO ()

--------------------------------------------------------------------------------
-- $ManagingSourceExecution
-- The execution state of a source can be queried. OpenAL provides a set of
-- functions that initiate state transitions causing sources to start and stop
-- execution.

--------------------------------------------------------------------------------

-- | Contains the current execution state of the given source. The initial state
-- of any source is 'Initial'.
--
-- Note that the 'Initial' state is not necessarily identical to the initial
-- state in which a source is created, because the other source attributes are
-- not automatically reset to their initial values. 'Initial' merely indicates
-- that the source can be executed using the 'play' command. A 'Stopped' or
-- 'Initial' source can be reset into the default configuration by using a
-- sequence of source commands as necessary. As the application has to specify
-- all relevant state anyway to create a useful source configuration, no reset
-- command is provided.

sourceState :: Source -> GettableStateVar SourceState
sourceState source =
   makeGettableStateVar
      (makeGetter dictSourceState GetSourceState source)

--------------------------------------------------------------------------------

-- | 'play' applied to an 'Initial' source will promote the source to 'Playing',
-- thus the data found in the buffer will be fed into the processing, starting
-- at the beginning. 'play' applied to a 'Playing' source will restart the
-- source from the beginning. It will not affect the configuration, and will
-- leave the source in 'Playing' state, but reset the sampling offset to the
-- beginning. 'play' applied to a 'Paused' source will resume processing using
-- the source state as preserved at the 'pause' operation. 'play' applied to a
-- 'Stopped' source will propagate it to 'Initial' then to 'Playing'
-- immediately.

play :: [Source] -> IO ()
play = withArraySizei alSourcePlayv

foreign import CALLCONV unsafe "alSourcePlayv"
   alSourcePlayv :: ALsizei -> Ptr Source -> IO ()

-- | 'pause' applied to an 'Initial' source is a legal NOP. 'pause' applied to a
-- 'Playing' source will change its state to 'Paused'. The source is exempt from
-- processing, its current state is preserved. 'pause' applied to a 'Paused'
-- source is a legal NOP. 'pause' applied to a 'Stopped' source is a legal NOP.

pause :: [Source] -> IO ()
pause = withArraySizei alSourcePausev

foreign import CALLCONV unsafe "alSourcePausev"
   alSourcePausev :: ALsizei -> Ptr Source -> IO ()

-- | 'stop' applied to an 'Initial' source is a legal NOP. 'stop' applied to a
-- 'Playing' source will change its state to 'Stopped'. The source is exempt
-- from processing, its current state is preserved. 'stop' applied to a 'Paused'
-- source will change its state to 'Stopped', with the same consequences as on a
-- 'Playing' source. 'stop' applied to a 'Stopped' source is a legal NOP.

stop :: [Source] -> IO ()
stop = withArraySizei alSourceStopv

foreign import CALLCONV unsafe "alSourceStopv"
   alSourceStopv :: ALsizei -> Ptr Source -> IO ()

-- | 'rewind' applied to an 'Initial' source is a legal NOP. 'rewind' applied to
-- a 'Playing' source will change its state to 'Stopped' then 'Initial'. The
-- source is exempt from processing: its current state is preserved, with the
-- exception of the sampling offset, which is reset to the beginning.  'rewind'
-- applied to a 'Paused' source will change its state to 'Initial', with the
-- same consequences as on a 'Playing' source.  'rewind' applied to an 'Stopped'
-- source promotes the source to 'Initial', resetting the sampling offset to the
-- beginning.

rewind :: [Source] -> IO ()
rewind = withArraySizei alSourceRewindv

foreign import CALLCONV unsafe "alSourceRewindv"
   alSourceRewindv :: ALsizei -> Ptr Source -> IO ()
