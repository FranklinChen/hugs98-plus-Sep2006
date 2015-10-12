--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.ALUT.Loaders
-- Copyright   :  (c) Sven Panne 2005
-- License     :  BSD-style (see the file libraries/ALUT/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Sound.ALUT.Loaders (
   Phase, Duration, SoundDataSource(..),
   createBuffer, createBufferData,
   bufferMIMETypes, bufferDataMIMETypes
)  where

import Foreign.C.String ( peekCString, withCString )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Storable ( Storable(peek) )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )
import Sound.ALUT.Config (
   alut_CreateBufferFromFile, alut_CreateBufferFromFileImage,
   alut_CreateBufferHelloWorld, alut_CreateBufferWaveform,
   alut_LoadMemoryFromFile, alut_LoadMemoryFromFileImage,
   alut_LoadMemoryHelloWorld, alut_LoadMemoryWaveform,
   alut_GetMIMETypes )
import Sound.ALUT.Constants (
   alut_WAVEFORM_SINE, alut_WAVEFORM_SQUARE, alut_WAVEFORM_SAWTOOTH,
   alut_WAVEFORM_IMPULSE, alut_WAVEFORM_WHITENOISE,
   alut_LOADER_BUFFER, alut_LOADER_MEMORY )
import Sound.ALUT.Errors ( makeBuffer, throwIfNullPtr )
import Sound.OpenAL.AL.BasicTypes ( ALsizei, ALenum, ALfloat )
import Sound.OpenAL.AL.Buffer ( Buffer, MemoryRegion(..), BufferData(..) )
import Sound.OpenAL.AL.Format ( unmarshalFormat )
import Sound.OpenAL.ALC.Context ( Frequency )
import System.IO ( FilePath )

--------------------------------------------------------------------------------

type Phase = Float

type Duration = Float

data SoundDataSource a =
     File FilePath
   | FileImage (MemoryRegion a)
   | HelloWorld
   | Sine Frequency Phase Duration
   | Square Frequency Phase Duration
   | Sawtooth Frequency Phase Duration
   | Impulse Frequency Phase Duration
   | WhiteNoise Duration
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq (SoundDataSource a)
instance Ord (SoundDataSource a)
instance Show (SoundDataSource a)
#else
   deriving ( Eq, Ord, Show )
#endif

--------------------------------------------------------------------------------

createBuffer :: SoundDataSource a -> IO Buffer
createBuffer src = makeBuffer "createBuffer" $ case src of
   File filePath -> withCString filePath alut_CreateBufferFromFile
   FileImage (MemoryRegion buf size) -> alut_CreateBufferFromFileImage buf size
   HelloWorld -> alut_CreateBufferHelloWorld
   Sine f p d -> alut_CreateBufferWaveform alut_WAVEFORM_SINE f p d 
   Square f p d -> alut_CreateBufferWaveform alut_WAVEFORM_SQUARE f p d
   Sawtooth f p d -> alut_CreateBufferWaveform alut_WAVEFORM_SAWTOOTH f p d
   Impulse f p d -> alut_CreateBufferWaveform alut_WAVEFORM_IMPULSE f p d
   WhiteNoise d -> alut_CreateBufferWaveform alut_WAVEFORM_WHITENOISE 1 0 d

--------------------------------------------------------------------------------

createBufferData :: SoundDataSource a -> IO (BufferData b)
createBufferData src = case src of
   File filePath -> withCString filePath $ \fp -> loadWith (alut_LoadMemoryFromFile fp)
   FileImage (MemoryRegion buf size) -> loadWith (alut_LoadMemoryFromFileImage buf size)
   HelloWorld -> loadWith alut_LoadMemoryHelloWorld
   Sine f p d -> loadWith (alut_LoadMemoryWaveform alut_WAVEFORM_SINE f p d)
   Square f p d -> loadWith (alut_LoadMemoryWaveform alut_WAVEFORM_SQUARE f p d)
   Sawtooth f p d -> loadWith (alut_LoadMemoryWaveform alut_WAVEFORM_SAWTOOTH f p d)
   Impulse f p d -> loadWith (alut_LoadMemoryWaveform alut_WAVEFORM_IMPULSE f p d)
   WhiteNoise d -> loadWith (alut_LoadMemoryWaveform alut_WAVEFORM_WHITENOISE 1 0 d)

loadWith :: (Ptr ALenum -> Ptr ALsizei -> Ptr ALfloat -> IO (Ptr b)) -> IO (BufferData b)
loadWith loader =
   alloca $ \formatBuf ->
      alloca $ \sizeBuf ->
         alloca $ \frequencyBuf -> do
            buf <- throwIfNullPtr "createBufferData" $
                      loader formatBuf sizeBuf frequencyBuf
            format <- peek formatBuf
            size <- peek sizeBuf
            frequency <- peek frequencyBuf
            return $ BufferData (MemoryRegion buf size) (unmarshalFormat format) frequency

--------------------------------------------------------------------------------

bufferMIMETypes :: GettableStateVar [String]
bufferMIMETypes = mimeTypes "bufferMIMETypes" alut_LOADER_BUFFER

bufferDataMIMETypes :: GettableStateVar [String]
bufferDataMIMETypes = mimeTypes "bufferDataMIMETypes" alut_LOADER_MEMORY

mimeTypes :: String -> ALenum -> GettableStateVar [String]
mimeTypes name loaderType =
   makeGettableStateVar $ do
      ts <- throwIfNullPtr name $ alut_GetMIMETypes loaderType
      fmap (splitBy (== ',')) $ peekCString ts

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p xs = case break p xs of
                (ys, []  ) -> [ys]
                (ys, _:zs) -> ys : splitBy p zs
