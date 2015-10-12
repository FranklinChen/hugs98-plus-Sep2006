-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.ALUT.Errors
-- Copyright   :  (c) Sven Panne 2005
-- License     :  BSD-style (see the file libraries/ALUT/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Sound.ALUT.Errors (
   throwIfALfalse, throwIfNullPtr, makeBuffer
)  where

import Control.Monad ( when )
import Data.Maybe ( fromJust, isNothing )
import Foreign.Ptr ( Ptr, nullPtr )
import Sound.OpenAL.AL.ALboolean ( unmarshalALboolean )
import Sound.OpenAL.AL.BasicTypes ( ALboolean, ALuint )
import Sound.OpenAL.AL.Buffer ( Buffer )
import Sound.OpenAL.AL.BufferInternal ( unmarshalBuffer )
import Sound.ALUT.Config ( alut_GetError, alut_GetErrorString )

--------------------------------------------------------------------------------

throwIf ::  (a -> Bool) -> String -> IO a -> IO a
throwIf failurePredicate name action = do
   returnValue <- action
   when (failurePredicate returnValue) $ do
      description <- alut_GetErrorString =<< alut_GetError
      ioError (userError (name ++ ": " ++ description))
   return returnValue

--------------------------------------------------------------------------------

throwIfALfalse :: String -> IO ALboolean -> IO ()
throwIfALfalse name action = do
   throwIf (not . unmarshalALboolean) name action
   return ()

--------------------------------------------------------------------------------

throwIfNullPtr :: String -> IO (Ptr a) -> IO (Ptr a)
throwIfNullPtr = throwIf (== nullPtr)

--------------------------------------------------------------------------------

makeBuffer :: String -> IO ALuint -> IO Buffer
makeBuffer name =
   fmap fromJust . throwIf isNothing name . fmap unmarshalBuffer
