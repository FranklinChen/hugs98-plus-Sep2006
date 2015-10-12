-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.ALboolean
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling ALboolean.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.ALboolean (
   marshalALboolean, unmarshalALboolean
) where

import Sound.OpenAL.AL.BasicTypes ( ALboolean )
import Sound.OpenAL.Constants ( al_FALSE, al_TRUE )

--------------------------------------------------------------------------------

marshalALboolean :: Bool -> ALboolean
marshalALboolean False = al_FALSE
marshalALboolean True  = al_TRUE

unmarshalALboolean :: ALboolean -> Bool
unmarshalALboolean = (/= al_FALSE)
