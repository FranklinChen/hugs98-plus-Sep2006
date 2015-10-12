-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.BufferInternal
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling Buffer.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.BufferInternal (
   Buffer, marshalBuffer, unmarshalBuffer
) where

import Foreign.Marshal.Array ( withArrayLen, peekArray, allocaArray )
import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL.GL.BufferObjects ( ObjectName(..) )
import Sound.OpenAL.AL.ALboolean ( unmarshalALboolean )
import Sound.OpenAL.AL.BasicTypes ( ALboolean, ALuint, ALsizei )
import Sound.OpenAL.AL.PeekPoke ( peek1, poke1 )

--------------------------------------------------------------------------------

-- | The abstract buffer type.

newtype Buffer = Buffer { bufferID :: ALuint }
   deriving ( Eq, Ord, Show )
 
-- | A dummy buffer.

nullBuffer :: Buffer
nullBuffer = Buffer 0

marshalBuffer :: Maybe Buffer -> ALuint
marshalBuffer = bufferID . maybe nullBuffer id

unmarshalBuffer :: ALuint -> Maybe Buffer
unmarshalBuffer b =
   if b == bufferID nullBuffer then Nothing else Just (Buffer b)

instance Storable Buffer where
   sizeOf    ~(Buffer b) = sizeOf b
   alignment ~(Buffer b) = alignment b
   peek                  = peek1 Buffer . castPtr
   poke ptr   (Buffer b) = poke1 (castPtr ptr) b

--------------------------------------------------------------------------------
-- This should really be in Sound.OpenAL.AL.Buffer, but we have it here to
-- avoid an orphan module.

instance ObjectName Buffer where
   genObjectNames n =
      allocaArray n $ \buf -> do
        alGenBuffers (fromIntegral n) buf
        peekArray n buf

   deleteObjectNames buffers =
      withArrayLen buffers $ alDeleteBuffers . fromIntegral

   isObjectName = fmap unmarshalALboolean . alIsBuffer

foreign import CALLCONV unsafe "alGenBuffers"
   alGenBuffers :: ALsizei -> Ptr Buffer -> IO ()

foreign import CALLCONV unsafe "alDeleteBuffers"
   alDeleteBuffers :: ALsizei -> Ptr Buffer -> IO ()

foreign import CALLCONV unsafe "alIsBuffer"
   alIsBuffer :: Buffer -> IO ALboolean
