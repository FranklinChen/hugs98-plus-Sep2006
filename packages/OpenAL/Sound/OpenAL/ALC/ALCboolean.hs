-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.ALC.ALCboolean
-- Copyright   :  (c) Sven Panne 2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling ALCboolean.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.ALC.ALCboolean (
   marshalALCboolean, unmarshalALCboolean
) where

import Sound.OpenAL.ALC.BasicTypes ( ALCboolean )
import Sound.OpenAL.Constants ( alc_FALSE, alc_TRUE )

--------------------------------------------------------------------------------

marshalALCboolean :: Bool -> ALCboolean
marshalALCboolean False = alc_FALSE
marshalALCboolean True  = alc_TRUE

unmarshalALCboolean :: ALCboolean -> Bool
unmarshalALCboolean = (/= alc_FALSE)
