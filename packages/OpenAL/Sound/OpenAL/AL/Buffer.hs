--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.Buffer
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to chapter 6 (Buffers) of the OpenAL Specification
-- and Reference (version 1.1).
--
-- A buffer encapsulates OpenAL state related to storing sample data. The
-- application can request and release buffer objects, and fill them with
-- data. Data can be supplied compressed and encoded as long as the format is
-- supported. Buffers can, internally, contain waveform data as uncompressed or
-- compressed samples.
-- 
-- Unlike source (see "Sound.OpenAL.AL.Source") and listener (see
-- "Sound.OpenAL.AL.Listener") objects, buffer objects can be shared among AL
-- contexts. Buffers are referenced by sources. A single buffer can be referred
-- to by multiple sources. This separation allows drivers and hardware to
-- optimize storage and processing where applicable.
-- 
-- The simplest supported format for buffer data is PCM. PCM data is assumed to
-- use the processor\'s native byte order. Other formats use the byte order
-- native to that format.
-- 
-- At this time, buffer states are defined for purposes of discussion. The
-- states described in this section are not exposed through the API (can not be
-- queried, or be set directly), and the state description used in the
-- implementation might differ from this.
--
-- A buffer is considered to be in one of the following states, with respect to
-- all sources:
--
-- [/unused/] The buffer is not included in any queue for any source. In
-- particular, the buffer is neither pending nor current for any source. The
-- buffer name can be deleted at this time.
--
-- [/processed/] The buffer is listed in the queue of at least one source, but
-- is neither pending nor current for any source. The buffer can be deleted as
-- soon as it has been unqueued for all sources it is queued with.
--
-- [/pending/] There is at least one source for which the buffer has been queued,
-- for which the buffer data has not yet been dereferenced. The buffer can only
-- be unqueued for those sources that have dereferenced the data in the buffer
-- in its entirety, and cannot be deleted or changed.
--
-- The buffer state is dependent on the state of all sources that is has been
-- queued for. A single queue occurrence of a buffer propagates the buffer state
-- (over all sources) from /unused/ to /processed/ or higher. Sources that are
-- in the 'Stopped' or 'Initial' states still have queue entries that cause
-- buffers to be /processed/.
-- 
-- A single queue entry with a single source for which the buffer is not yet
-- /processed/ propagates the buffer\'s queuing state to /pending/.
-- 
-- Buffers that are /processed/ for a given source can be unqueued from that
-- source\'s queue. Buffers that have been unqueued from all sources are
-- /unused/. Buffers that are /unused/ can be deleted, or changed by writing
-- 'bufferData'.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.Buffer (
   Buffer, MemoryRegion(..), Format(..), BufferData(..), bufferData
) where

import Foreign.Marshal.Alloc ( alloca )
import Foreign.Ptr ( Ptr, nullPtr )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )
import Sound.OpenAL.AL.BufferInternal ( Buffer )
import Sound.OpenAL.AL.BasicTypes ( ALint, ALsizei, ALenum )
import Sound.OpenAL.AL.Format ( Format(..), marshalFormat )
import Sound.OpenAL.AL.PeekPoke ( peek1 )
import Sound.OpenAL.ALC.Context ( Frequency )
import Sound.OpenAL.Constants ( al_FREQUENCY, al_SIZE, al_BITS, al_CHANNELS )

#ifdef __HADDOCK__
import Sound.OpenAL.AL.SourceState ( SourceState(..) )
import Sound.OpenAL.AL.Errors ( ALErrorCategory(..) )
#endif

--------------------------------------------------------------------------------

data MemoryRegion a = MemoryRegion (Ptr a) ALsizei
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq (MemoryRegion a)
instance Ord (MemoryRegion a)
instance Show (MemoryRegion a)
#else
   deriving ( Eq, Ord, Show )
#endif

data BufferData a = BufferData (MemoryRegion a) Format Frequency
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq (BufferData a)
instance Ord (BufferData a)
instance Show (BufferData a)
#else
   deriving ( Eq, Ord, Show )
#endif

--------------------------------------------------------------------------------

-- | A special case of buffer state is the actual sound sample data stored in
-- association with the buffer. Applications can specify sample data using
-- 'bufferData'.
--
-- The data specified is copied to an internal software, or if possible,
-- hardware buffer. The implementation is free to apply decompression,
-- conversion, resampling, and filtering as needed. The internal format of the
-- buffer is not exposed to the application, and not accessible.
--
-- Buffers containing audio data with more than one channel will be played
-- without 3D spatialization features, these formats are normally used for
-- background music. Applications should always check for an error condition
-- after attempting to specify buffer data in case an implementation has to
-- generate an 'ALOutOfMemory' or a conversion related 'ALInvalidValue' error.
-- The application is free to reuse the memory specified by the data pointer
-- once 'bufferData' is set. The implementation has to dereference, e.g. copy,
-- the data while accessing 'bufferData' execution.

bufferData :: Buffer -> StateVar (BufferData a)
bufferData buffer = makeStateVar (getBufferData buffer) (setBufferData buffer)

getBufferData :: Buffer -> IO (BufferData a)
getBufferData buffer = do
   channels <- getBufferi buffer Channels
   bits <- getBufferi buffer Bits
   raw <- return nullPtr -- ToDo: AL_DATA query missing!!!
   size <- getBufferi buffer Size
   frequency <- getBufferi buffer Frequency
   return $ BufferData (MemoryRegion  raw size) (makeFormat channels bits) frequency

setBufferData :: Buffer -> BufferData a -> IO ()
setBufferData buffer (BufferData (MemoryRegion raw size) format frequency) =
      alBufferData buffer (marshalFormat format) raw size (round frequency)

foreign import CALLCONV unsafe "alBufferData"
   alBufferData :: Buffer -> ALenum -> Ptr a -> ALsizei -> ALsizei -> IO ()

--------------------------------------------------------------------------------

-- ToDo: What about IMAADPCMMono16, IMAADPCMStereo16, Vorbis...?
makeFormat :: ALint -> ALint -> Format
makeFormat 1  8 = Mono8
makeFormat 2  8 = Stereo8
makeFormat 1 16 = Mono16
makeFormat 2 16 = Stereo16
makeFormat channels bits =
   error ("makeFormat: illegal values " ++ show (channels, bits))

--------------------------------------------------------------------------------

data BufferQuery =
     Frequency
   | Size
   | Bits
   | Channels

marshalBufferQuery :: BufferQuery -> ALenum
marshalBufferQuery x = case x of
   Frequency -> al_FREQUENCY
   Size -> al_SIZE
   Bits -> al_BITS
   Channels -> al_CHANNELS

--------------------------------------------------------------------------------

getBufferi :: Num a => Buffer -> BufferQuery -> IO a
getBufferi buffer query =
   alloca $ \buf -> do
      alGetBufferi buffer (marshalBufferQuery query) buf
      peek1 fromIntegral buf

foreign import CALLCONV unsafe "alGetBufferi"
   alGetBufferi :: Buffer -> ALenum -> Ptr ALint -> IO ()
