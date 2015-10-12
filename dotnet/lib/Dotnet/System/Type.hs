module Dotnet.System.Type 
	( module Dotnet.System.Type,
	  module Dotnet.System.TypeTy
	) where

import Dotnet.System.TypeTy

foreign import dotnet
  "static Dotnet.System.Type.GetType"
  getType :: String -> IO (Type ())
