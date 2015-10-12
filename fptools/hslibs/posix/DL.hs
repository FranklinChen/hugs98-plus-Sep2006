{-# OPTIONS -#include "HsPosix.h" #-}
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
--  Usage:
--  ******
--  
--  Let's assume you want to open a local shared library 'foo' (./libfoo.so)
--  offering a function
--    char * mogrify (char*,int)
--  and invoke str = mogrify("test",1):
-- 
--  type Fun = CString -> Int -> IO CString
--  foreign import dynamic unsafe fun__ :: FunPtr Fun -> Fun
-- 
--  withModule (Just ".") ("libfoo.so") [RTLD_NOW] $ \ mod -> do
--     funptr <- moduleSymbol mod "mogrify"
--     let fun = fun__ funptr
--     withCString "test" $ \ str -> do
--       strptr <- fun str 1
--       strstr <- peekCString strptr
--       ...


module DL
    {-# DEPRECATED "Use System.Posix.DynamicLinker and System.Posix.DynamicLinker.Prim.Module instead" #-}
    ( module DLPrim
    , moduleOpen             -- :: String -> ModuleFlags -> IO Module
    , moduleSymbol           -- :: Source -> String -> IO (FunPtr a)
    , moduleClose            -- :: Module -> IO Bool
    , moduleError            -- :: IO String
    , withModule             -- :: Maybe String 
                             -- -> String 
	                     -- -> [ModuleFlags ]
			     -- -> (Module -> IO a) 
			     -- -> IO a
    , withModule_            -- :: Maybe String 
 			     -- -> String 
 			     -- -> [ModuleFlags] 
 			     -- -> (Module -> IO a) 
 			     -- -> IO ()
    )
where

import DLPrim
import Foreign.Ptr	( Ptr, nullPtr, FunPtr, nullFunPtr )
import Foreign.C.String	( CString, withCString, peekCString )

-- abstract handle for dynamically loaded module (EXPORTED)
--
newtype Module = Module (Ptr ())

unModule              :: Module -> (Ptr ())
unModule (Module adr)  = adr

-- Opens a module (EXPORTED)
--

moduleOpen :: String -> [ModuleFlags] -> IO Module
moduleOpen mod flags = do
  modPtr <- withCString mod $ \ modAddr -> dlopen modAddr (packModuleFlags flags)
  if (modPtr == nullPtr)
      then moduleError >>= \ err -> ioError (userError ("dlopen: " ++ err))
      else return $ Module modPtr

-- Gets a symbol pointer from a module (EXPORTED)
--
moduleSymbol :: Module -> String -> IO (FunPtr a)
moduleSymbol mod sym = do
  withCString sym $ \ symPtr -> do
    ptr <- dlsym (unModule mod) symPtr
    if (ptr /= nullFunPtr)
      then return ptr
      else moduleError >>= \ err -> ioError (userError ("dlsym: " ++ err))

-- Closes a module (EXPORTED)
-- 
moduleClose     :: Module -> IO ()
moduleClose mod  = dlclose (unModule mod)

-- Gets a string describing the last module error (EXPORTED)
-- 
moduleError :: IO String
moduleError  = peekCString =<< dlerror


-- Convenience function, cares for module open- & closing
-- additionally returns status of `moduleClose' (EXPORTED)
-- 
withModule :: Maybe String 
           -> String 
	   -> [ModuleFlags]
           -> (Module -> IO a) 
	   -> IO a
withModule dir mod flags p = do
  let modPath = case dir of
                  Nothing -> mod
	          Just p  -> p ++ if ((head (reverse p)) == '/')
                                       then mod
				       else ('/':mod)
  mod <- moduleOpen modPath flags
  result <- p mod
  moduleClose mod
  return result

withModule_ :: Maybe String 
            -> String 
	    -> [ModuleFlags]
            -> (Module -> IO a) 
	    -> IO ()
withModule_ dir mod flags p = withModule dir mod flags p >>= \ _ -> return ()
