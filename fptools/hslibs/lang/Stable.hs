module Stable
  {-# DEPRECATED "This functionality is now available from Foreign.StablePtr and System.Mem.StableName" #-} 
  (module Foreign.StablePtr, module System.Mem.StableName) where
import Foreign.StablePtr
import System.Mem.StableName

