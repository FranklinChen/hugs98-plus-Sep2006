{-
   HelloWorld.hs (adapted from hello_world.c in freealut)
   Copyright (c) Sven Panne 2005 <sven.panne@aedion.de>
   This file is part of the ALUT package & distributed under a BSD-style license
   See the file libraries/ALUT/LICENSE
-}

import Sound.ALUT

-- This is the Haskell version of the 'Hello World' program from the ALUT
-- reference manual.

main :: IO ()
main =
   withProgNameAndArgs runALUT $ \_progName _args -> do
      helloBuffer <- createBuffer HelloWorld
      [helloSource] <- genObjectNames 1
      buffer helloSource $= Just helloBuffer
      play [helloSource]
      sleep 1
