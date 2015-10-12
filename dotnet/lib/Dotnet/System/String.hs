module Dotnet.System.String 
	( module Dotnet.System.String,
	  module Dotnet.System.StringTy
 	) where

import Dotnet hiding ( Object, new )
import Dotnet.System.Object
import Dotnet.System.StringTy

new :: String -> IO (StringTy ())
new str = newString str

foreign import dotnet
  "static field Dotnet.System.String.Empty"
  empty :: IO (StringTy a)

foreign import dotnet
  "method Dotnet.System.String.get_Chars"
  charAt :: Int -> StringTy a -> IO Char

foreign import dotnet
  "method Dotnet.System.String.get_Length"
  lengthString :: StringTy a -> IO Int

foreign import dotnet
  "method Dotnet.System.String.Clone"
  clone :: StringTy a -> IO (StringTy a)

foreign import dotnet
  "method Dotnet.System.String.EndsWith"
  endsWith :: String -> StringTy a -> IO Bool

foreign import dotnet
  "method Dotnet.System.String.StartsWith"
  startsWith :: String -> StringTy a -> IO Bool

