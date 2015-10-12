-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.Format
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling Format.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.Format (
   Format(..), marshalFormat, unmarshalFormat
) where

import Sound.OpenAL.AL.BasicTypes ( ALenum )
import Sound.OpenAL.Constants (
   al_FORMAT_MONO8, al_FORMAT_MONO16, al_FORMAT_STEREO8, al_FORMAT_STEREO16 )

--------------------------------------------------------------------------------

-- | Valid sound formats. An implementation may expose other formats, see
-- "Sound.OpenAL.ALC.Extensions" for information on determining if additional
-- formats are supported.

data Format =
     Mono8
   | Mono16
   | Stereo8
   | Stereo16
   deriving ( Eq, Ord, Show )

marshalFormat :: Format -> ALenum
marshalFormat x = case x of
   Mono8 -> al_FORMAT_MONO8
   Mono16 -> al_FORMAT_MONO16
   Stereo8 -> al_FORMAT_STEREO8
   Stereo16 -> al_FORMAT_STEREO16

unmarshalFormat :: ALenum -> Format
unmarshalFormat x
   | x == al_FORMAT_MONO8 = Mono8
   | x == al_FORMAT_MONO16 = Mono16
   | x == al_FORMAT_STEREO8 = Stereo8
   | x == al_FORMAT_STEREO16 = Stereo16
   | otherwise = error ("unmarshalFormat: illegal value " ++ show x)
