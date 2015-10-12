module Dotnet.System.Xml.XPath.XPathNamespaceScope where

import Dotnet
import qualified IOExts
import qualified Dotnet.System.Type
import qualified Dotnet.System.Enum

data XPathNamespaceScope_ a
type XPathNamespaceScope a = Dotnet.System.Enum.Enum (XPathNamespaceScope_ a)

data XPathNamespaceScopeTy
 = All
 | ExcludeXml
 | Local
  deriving ( Enum, Show, Read )
toXPathNamespaceScope :: XPathNamespaceScopeTy -> XPathNamespaceScope ()
toXPathNamespaceScope tag = IOExts.unsafePerformIO (Dotnet.System.Enum.parse (IOExts.unsafePerformIO (Dotnet.System.Type.getType "System.Xml.XPath.XPathNamespaceScope, System.Xml, Version=1.0.3300.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")) (show tag))

fromXPathNamespaceScope :: XPathNamespaceScope () -> XPathNamespaceScopeTy
fromXPathNamespaceScope obj = IOExts.unsafePerformIO (toString obj >>= return.read)

