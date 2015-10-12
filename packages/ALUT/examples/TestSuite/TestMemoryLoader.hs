{-
   TestMemoryLoader.hs (adapted from test_memoryloader.c in freealut)
   Copyright (c) Sven Panne 2005 <sven.panne@aedion.de>
   This file is part of the ALUT package & distributed under a BSD-style license
   See the file libraries/ALUT/LICENSE
-}

import Control.Exception ( bracket )
import Control.Monad ( when, unless )
import Data.List ( intersperse )
import Foreign.Marshal.Alloc ( allocaBytes )
import Sound.ALUT
import System.Exit ( exitFailure )
import System.IO (
   openBinaryFile, IOMode(ReadMode), hClose, hFileSize, hGetBuf, hPutStrLn,
   stderr )

-- This program loads and plays a variety of files from memory, basically a
-- modified version of TestFileLoader.hs.

withFileContents :: FilePath -> (MemoryRegion a -> IO b) -> IO b
withFileContents filePath action =
   bracket (openBinaryFile filePath ReadMode) hClose $ \handle -> do
      numBytes <- fmap fromIntegral (hFileSize handle)
      allocaBytes numBytes $ \buf -> do
         bytesRead <- hGetBuf handle buf numBytes
         when (bytesRead /= numBytes) $
            ioError (userError "hGetBuf")
         action (MemoryRegion buf (fromIntegral numBytes))

playFile :: FilePath -> IO ()
playFile fileName = do
   -- Load the sound file into memory and create an AL buffer from it.
   buf <- withFileContents fileName (createBuffer . FileImage)

   -- Generate a single source, attach the buffer to it and start playing.
   [source] <- genObjectNames 1
   buffer source $= Just buf
   play [source]

   -- Normally nothing should go wrong above, but one never knows...
   errs <- get alErrors
   unless (null errs) $ do
      hPutStrLn stderr (concat (intersperse "," [ d | ALError _ d <- errs ]))
      exitFailure

   -- Check every 0.1 seconds if the sound is still playing.
   let waitWhilePlaying = do
          sleep 0.1
          state <- get (sourceState source)
          when (state == Playing) $
             waitWhilePlaying
   waitWhilePlaying

main :: IO ()
main =
   -- Initialise ALUT and eat any ALUT-specific commandline flags.
   withProgNameAndArgs runALUT $ \_progName _args -> do

     -- If everything is OK, play the sound files and exit when finished. Note
     -- that we can not play raw sound files from memory because the format
     -- can't be guessed without a file name.
     mapM_ playFile [ "file1.wav", "file2.au" ]
