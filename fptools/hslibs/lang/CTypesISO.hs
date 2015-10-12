module CTypesISO
  {-# DEPRECATED "This library will go away soon; use Foreign.C.Types instead" #-} 
  (module Foreign.C.Types) where
import Foreign.C.Types
#ifndef __NHC__
   ( CPtrdiff, CSize, CWchar, CSigAtomic , CClock, CTime )
#else
-- For nhc98, these are exported non-abstractly to work around
-- an interface-file problem.
   ( CPtrdiff(..), CSize(..), CWchar(..), CSigAtomic(..), CClock(..),   CTime(..)
#endif
