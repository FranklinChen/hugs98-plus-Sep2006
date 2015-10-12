--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.ALUT.Version
-- Copyright   :  (c) Sven Panne 2005
-- License     :  BSD-style (see the file libraries/ALUT/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Sound.ALUT.Version (
   alutAPIVersion, alutVersion
)  where

import Control.Monad ( liftM2 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )
import Sound.OpenAL.AL.BasicTypes ( ALint )
import Sound.ALUT.Config ( alut_GetMajorVersion, alut_GetMinorVersion )
import Sound.ALUT.Constants ( alut_API_MAJOR_VERSION, alut_API_MINOR_VERSION )

--------------------------------------------------------------------------------

alutAPIVersion :: String
alutAPIVersion = makeVersionString alut_API_MAJOR_VERSION alut_API_MINOR_VERSION

makeVersionString :: ALint -> ALint -> String
makeVersionString major minor = show major ++ "." ++ show minor

--------------------------------------------------------------------------------

alutVersion :: GettableStateVar String
alutVersion =
   makeGettableStateVar $
      liftM2 makeVersionString alut_GetMajorVersion alut_GetMinorVersion
