module Dotnet.System.Xml.XmlResolver where

import Dotnet
import qualified Dotnet.System.Object
--import Dotnet.System.Net.ICredentials
import Dotnet.System.Uri
import Dotnet.System.Type

data XmlResolver_ a
type XmlResolver a = Dotnet.System.Object.Object (XmlResolver_ a)

{-
foreign import dotnet
  "method Dotnet.System.Xml.XmlResolver.set_Credentials"
  set_Credentials :: Dotnet.System.Net.ICredentials.ICredentials a0 -> XmlResolver obj -> IO (())
-}

foreign import dotnet
  "method Dotnet.System.Xml.XmlResolver.ResolveUri"
  resolveUri :: Dotnet.System.Uri.Uri a0 -> String -> XmlResolver obj -> IO (Dotnet.System.Uri.Uri a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlResolver.GetEntity"
  getEntity :: Dotnet.System.Uri.Uri a0 -> String -> Dotnet.System.Type.Type a2 -> XmlResolver obj -> IO (Dotnet.System.Object.Object a3)


