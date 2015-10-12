--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.ALUT.Initialization
-- Copyright   :  (c) Sven Panne 2005
-- License     :  BSD-style (see the file libraries/ALUT/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Sound.ALUT.Initialization (
   ArgumentConsumer, Runner, runALUT, runALUTUsingCurrentContext,
   withProgNameAndArgs
)  where

import Data.List ( genericLength )
import Foreign.C.String ( CString, withCString, peekCString )
import Foreign.C.Types ( CInt )
import Foreign.Marshal.Array ( withArray0, peekArray )
import Foreign.Marshal.Utils ( with, withMany )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Storable ( Storable(peek) )
import Sound.ALUT.Errors ( throwIfALfalse )
import Sound.ALUT.Config ( alut_Init, alut_InitWithoutContext, alut_Exit )
import Sound.OpenAL.AL.BasicTypes ( ALboolean )
import System.Environment ( getProgName, getArgs )

-- Ugly: I see something like this in almost every package now...
#ifdef __NHC__
import IO ( bracket )

finally :: IO a -> IO b -> IO a
action `finally` sequel = bracket (return ()) (const sequel) (const action)

#else
import Control.Exception ( finally )
#endif

--------------------------------------------------------------------------------

type ArgumentConsumer a = String -> [String] -> a

type Runner a = ArgumentConsumer (IO a) -> IO a

--------------------------------------------------------------------------------

runALUT :: ArgumentConsumer (Runner a)
runALUT = runner "runALUT" alut_Init

--------------------------------------------------------------------------------

runALUTUsingCurrentContext :: ArgumentConsumer (Runner a)
runALUTUsingCurrentContext =
   runner "runALUTUsingCurrentContext" alut_InitWithoutContext

--------------------------------------------------------------------------------

runner :: String -> (Ptr CInt -> Ptr CString -> IO ALboolean) -> String
       -> [String] -> Runner a
runner name initAction progName args action =
   with (1 + genericLength args) $ \argcBuf ->
      withMany withCString (progName : args) $ \argvPtrs ->
         withArray0 nullPtr argvPtrs $ \argvBuf -> do
            throwIfALfalse name $ initAction argcBuf argvBuf
            newArgc <- peek argcBuf
            newArgvPtrs <- peekArray (fromIntegral newArgc) argvBuf
            newArgv <- mapM peekCString newArgvPtrs
            action (head newArgv) (tail newArgv)
               `finally` throwIfALfalse name alut_Exit

--------------------------------------------------------------------------------

withProgNameAndArgs :: (ArgumentConsumer (Runner a)) -> Runner a
withProgNameAndArgs alutRunner action = do
   n <- getProgName
   a <- getArgs
   alutRunner n a action
