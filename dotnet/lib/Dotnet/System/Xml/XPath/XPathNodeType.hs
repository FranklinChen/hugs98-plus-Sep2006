module Dotnet.System.Xml.XPath.XPathNodeType where

import Dotnet
import qualified IOExts
import qualified Dotnet.System.Type
import qualified Dotnet.System.Enum

data XPathNodeType_ a
type XPathNodeType a = Dotnet.System.Enum.Enum (XPathNodeType_ a)

data XPathNodeTypeTy
 = Root
 | Element
 | Attribute
 | Namespace
 | Text
 | SignificantWhitespace
 | Whitespace
 | ProcessingInstruction
 | Comment
 | All
  deriving ( Enum, Show, Read )
toXPathNodeType :: XPathNodeTypeTy -> XPathNodeType ()
toXPathNodeType tag = IOExts.unsafePerformIO (Dotnet.System.Enum.parse (IOExts.unsafePerformIO (Dotnet.System.Type.getType "System.Xml.XPath.XPathNodeType, System.Xml, Version=1.0.3300.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")) (show tag))

fromXPathNodeType :: XPathNodeType () -> XPathNodeTypeTy
fromXPathNodeType obj = IOExts.unsafePerformIO (toString obj >>= return.read)

