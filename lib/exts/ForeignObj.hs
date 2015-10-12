module ForeignObj( ForeignObj, module ForeignObj ) where

import Prelude
-- data ForeignObj -- in Prelude

-- recently renamed
newForeignObj = makeForeignObj

primitive newForeignPtr_ :: Addr{-free-} -> IO ForeignObj
primitive addForeignPtrFinalizer :: ForeignObj -> Addr{-free-} -> IO ()
primitive writeForeignObj :: ForeignObj -> Addr -> IO ()
primitive eqForeignObj    :: ForeignObj -> ForeignObj -> Bool

makeForeignObj addr finalizer = do
    fo <- newForeignPtr_ addr
    addForeignPtrFinalizer fo finalizer
    return fo

instance Eq ForeignObj where (==) = eqForeignObj
