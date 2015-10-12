module Dotnet.System.Object 
	(module Dotnet.System.Object, 
         module Dotnet.System.ObjectTy
	 ) where

import qualified Dotnet ( Object )
import Dotnet hiding ( Object )
import Dotnet.System.TypeTy
import Dotnet.System.ObjectTy
import Dotnet.System.StringTy

foreign import dotnet
  "method Dotnet.System.Object.Equals"
  equals :: Object a -> Object b -> IO Bool

foreign import dotnet
  "method Dotnet.System.Object.GetHashCode"
  getHashCode :: Object a -> IO Int

foreign import dotnet
  "method Dotnet.System.Object.GetType"
  getType :: Object a -> IO (Type ())

foreign import dotnet
  "method Dotnet.System.Object.MemberwiseClone"
  memberwiseClone :: Object a -> IO (Type a)

foreign import dotnet
  "static method Dotnet.System.Object.ReferenceEquals"
  referenceEquals :: Object a -> Object b -> IO Bool

foreign import dotnet
  "method Dotnet.System.Object.ToString"
  toString :: Object a -> IO String
