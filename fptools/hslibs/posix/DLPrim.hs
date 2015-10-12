--  Haskell Binding for dl{open,sym,...}          -*-haskell-*-
--
--  Author : Volker Stolz <stolz@i2.informatik.rwth-aachen.de>
--
--  Created: 2001-11-22
--
--  Derived from GModule.chs by M.Weber & M.Chakravarty which is part of c2hs
--  I left the API more or less the same, mostly the flags are different.
--
--  License: BSD
--

module DLPrim
  {-# DEPRECATED "Use System.Posix.DynamicLinker.Prim instead" #-}
  (
  dlopen,
  dlsym,
  dlerror,
  dlclose,
  -- dlAddr, -- XXX NYI
  haveRtldNext,
  haveRtldLocal,
  packModuleFlags,
  ModuleFlags,
  RTLDFlags(..),
  Source(..)
 )

where

import Foreign.Ptr	( Ptr, FunPtr )
import Foreign.C.Types	( CInt )
import Foreign.C.String	( CString )
import System.Posix.DynamicLinker.Prim ( haveRtldNext, haveRtldLocal,
			  RTLDFlags(..), packRTLDFlags )

-- data type definition
-- --------------------

-- flags passed to `moduleOpen' (EXPORTED)
--
type ModuleFlags = RTLDFlags

foreign import ccall unsafe "dlopen" dlopen :: CString -> CInt -> IO (Ptr ())
foreign import ccall unsafe "dlsym"  dlsym  :: (Ptr ()) -> CString -> IO (FunPtr a)
foreign import ccall unsafe "dlerror" dlerror :: IO CString
foreign import ccall unsafe "dlclose" dlclose :: (Ptr ()) -> IO ()

packModuleFlags :: [ModuleFlags] -> CInt
packModuleFlags = packRTLDFlags

data Source = Null | Next | Default | Name CString
