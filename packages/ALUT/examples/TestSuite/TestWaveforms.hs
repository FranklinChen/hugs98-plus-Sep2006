{-
   TestWaveforms.hs (adapted from test_waveforms.c in freealut)
   Copyright (c) Sven Panne 2005 <sven.panne@aedion.de>
   This file is part of the ALUT package & distributed under a BSD-style license
   See the file libraries/ALUT/LICENSE
-}

import Sound.ALUT

-- This program plays a 440Hz tone using a variety of waveforms.

playTone :: (Frequency -> Phase -> Duration -> SoundDataSource a) -> IO ()
playTone soundDataSource = do
   buf <- createBuffer (soundDataSource 440 0 1)
   [source] <- genObjectNames 1
   buffer source $= Just buf
   play [source]
   sleep 1

main :: IO ()
main =
   withProgNameAndArgs runALUT $ \_progName _args ->
      mapM_ playTone [ Sine, Square, Sawtooth, (const (const WhiteNoise)), Impulse ]
