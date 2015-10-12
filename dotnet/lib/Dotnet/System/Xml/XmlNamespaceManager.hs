module Dotnet.System.Xml.XmlNamespaceManager where

import Dotnet
import qualified Dotnet.System.Object
import Dotnet.System.Collections.IEnumerator
import Dotnet.System.Xml.XmlNameTable

data XmlNamespaceManager_ a
type XmlNamespaceManager a = Dotnet.System.Object.Object (XmlNamespaceManager_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamespaceManager.GetEnumerator"
  getEnumerator :: XmlNamespaceManager obj -> IO (Dotnet.System.Collections.IEnumerator.IEnumerator a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamespaceManager.HasNamespace"
  hasNamespace :: String -> XmlNamespaceManager obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamespaceManager.LookupPrefix"
  lookupPrefix :: String -> XmlNamespaceManager obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamespaceManager.LookupNamespace"
  lookupNamespace :: String -> XmlNamespaceManager obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamespaceManager.RemoveNamespace"
  removeNamespace :: String -> String -> XmlNamespaceManager obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamespaceManager.AddNamespace"
  addNamespace :: String -> String -> XmlNamespaceManager obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamespaceManager.PopScope"
  popScope :: XmlNamespaceManager obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamespaceManager.PushScope"
  pushScope :: XmlNamespaceManager obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamespaceManager.get_DefaultNamespace"
  get_DefaultNamespace :: XmlNamespaceManager obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamespaceManager.get_NameTable"
  get_NameTable :: XmlNamespaceManager obj -> IO (Dotnet.System.Xml.XmlNameTable.XmlNameTable a0)


