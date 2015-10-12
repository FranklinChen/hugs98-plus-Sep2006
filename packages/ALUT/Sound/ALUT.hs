--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.ALUT
-- Copyright   :  (c) Sven Panne 2005
-- License     :  BSD-style (see the file libraries/ALUT/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- A Haskell binding for the OpenAL Utility Toolkit, which makes managing of
-- OpenAL contexts, loading sounds in various formats and creating waveforms
-- very easy. For more information about the C library on which this binding is
-- based, please see: <http://www.openal.org/openal_webstf/specs/alut.html>.
--
--------------------------------------------------------------------------------

module Sound.ALUT (
   module Sound.OpenAL,

   module Sound.ALUT.Initialization,
   module Sound.ALUT.Loaders,
   module Sound.ALUT.Version,
   module Sound.ALUT.Sleep
)  where

import Sound.OpenAL

import Sound.ALUT.Initialization
import Sound.ALUT.Loaders
import Sound.ALUT.Version
import Sound.ALUT.Sleep
