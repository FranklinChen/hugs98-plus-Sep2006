{-
   PlayFile.hs (adapted from playfile.c in freealut)
   Copyright (c) Sven Panne 2005 <sven.panne@aedion.de>
   This file is part of the ALUT package & distributed under a BSD-style license
   See the file libraries/ALUT/LICENSE
-}

import Control.Monad ( when, unless )
import Data.List ( intersperse )
import Sound.ALUT
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )

-- This program loads and plays a variety of files.

playFile :: FilePath -> IO ()
playFile fileName = do
   -- Create an AL buffer from the given sound file.
   buf <- createBuffer (File fileName)

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
main = do
   -- Initialise ALUT and eat any ALUT-specific commandline flags.
   withProgNameAndArgs runALUT $ \progName args -> do

      -- Check for correct usage.
      unless (length args == 1) $ do
         hPutStrLn stderr ("usage: " ++ progName ++ " <fileName>")
         exitFailure

      -- If everything is OK, play the sound file and exit when finished.
      playFile (head args)
