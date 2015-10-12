--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.ALUT.Sleep
-- Copyright   :  (c) Sven Panne 2005
-- License     :  BSD-style (see the file libraries/ALUT/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Sound.ALUT.Sleep (
   sleep
)  where

import Sound.ALUT.Config ( alut_Sleep )
import Sound.ALUT.Errors ( throwIfALfalse )
import Sound.ALUT.Loaders ( Duration )

--------------------------------------------------------------------------------

sleep :: Duration -> IO ()
sleep = throwIfALfalse "sleep" . alut_Sleep
