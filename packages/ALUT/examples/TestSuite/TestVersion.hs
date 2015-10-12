{-
   TestVersion.hs (adapted from test_version.c in freealut)
   Copyright (c) Sven Panne 2005 <sven.panne@aedion.de>
   This file is part of the ALUT package & distributed under a BSD-style license
   See the file libraries/ALUT/LICENSE
-}

import Control.Monad ( unless )
import Sound.ALUT
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )

-- This program checks that the version of OpenAL in the library agrees with the
-- header file we're compiled against.

main :: IO ()
main =
   withProgNameAndArgs runALUT $ \_progName _args -> do
      av <- get alutVersion
      unless (av == alutAPIVersion) $ do
         hPutStrLn stderr ("WARNING: The ALUT library is version " ++ av ++ ".x but the ALUT binding says it's " ++ alutAPIVersion ++ ".x!")
         exitFailure
      hPutStrLn stderr ("The ALUT library is at version " ++ av ++ ".x")
