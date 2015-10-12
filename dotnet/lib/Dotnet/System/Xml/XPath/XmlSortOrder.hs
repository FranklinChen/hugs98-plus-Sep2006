module Dotnet.System.Xml.XPath.XmlSortOrder where

import Dotnet
import qualified IOExts
import qualified Dotnet.System.Type
import qualified Dotnet.System.Enum

data XmlSortOrder_ a
type XmlSortOrder a = Dotnet.System.Enum.Enum (XmlSortOrder_ a)

data XmlSortOrderTy
 = Ascending
 | Descending
  deriving ( Enum, Show, Read )
toXmlSortOrder :: XmlSortOrderTy -> XmlSortOrder ()
toXmlSortOrder tag = IOExts.unsafePerformIO (Dotnet.System.Enum.parse (IOExts.unsafePerformIO (Dotnet.System.Type.getType "System.Xml.XPath.XmlSortOrder, System.Xml, Version=1.0.3300.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")) (show tag))

fromXmlSortOrder :: XmlSortOrder () -> XmlSortOrderTy
fromXmlSortOrder obj = IOExts.unsafePerformIO (toString obj >>= return.read)

