-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.ALUT.Constants
-- Copyright   :  (c) Sven Panne 2005
-- License     :  BSD-style (see the file libraries/ALUT/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Sound.ALUT.Constants where

import Sound.OpenAL.AL.BasicTypes ( ALint, ALenum )

--------------------------------------------------------------------------------

#include "HsALUTConfig.h"

--------------------------------------------------------------------------------

alut_API_MAJOR_VERSION, alut_API_MINOR_VERSION :: ALint
alut_API_MAJOR_VERSION               = CONST_ALUT_API_MAJOR_VERSION
alut_API_MINOR_VERSION               = CONST_ALUT_API_MINOR_VERSION

--------------------------------------------------------------------------------

alut_WAVEFORM_SINE, alut_WAVEFORM_SQUARE, alut_WAVEFORM_SAWTOOTH,
   alut_WAVEFORM_IMPULSE, alut_WAVEFORM_WHITENOISE :: ALenum
alut_WAVEFORM_SINE                   = CONST_ALUT_WAVEFORM_SINE
alut_WAVEFORM_SQUARE                 = CONST_ALUT_WAVEFORM_SQUARE
alut_WAVEFORM_SAWTOOTH               = CONST_ALUT_WAVEFORM_SAWTOOTH
alut_WAVEFORM_IMPULSE                = CONST_ALUT_WAVEFORM_IMPULSE
alut_WAVEFORM_WHITENOISE             = CONST_ALUT_WAVEFORM_WHITENOISE

--------------------------------------------------------------------------------

alut_LOADER_BUFFER, alut_LOADER_MEMORY :: ALenum
alut_LOADER_BUFFER                   = CONST_ALUT_LOADER_BUFFER
alut_LOADER_MEMORY                   = CONST_ALUT_LOADER_MEMORY
