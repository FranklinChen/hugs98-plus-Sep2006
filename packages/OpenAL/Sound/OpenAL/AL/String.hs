-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.String
-- Copyright   :  (c) Sven Panne 2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.String (
   withALString, peekALString
) where

import Foreign.C.String ( withCString, peekCString )
import Foreign.Ptr ( Ptr, castPtr )
import Sound.OpenAL.AL.BasicTypes ( ALchar )

--------------------------------------------------------------------------------

-- AL uses "Ptr ALchar" instead of "CString" for strings, so some wrappers
-- are quite handy.

withALString :: String -> (Ptr ALchar -> IO a) -> IO a
withALString str action = withCString str (action . castPtr)

peekALString :: Ptr ALchar -> IO String
peekALString = peekCString . castPtr
