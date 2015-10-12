module Dotnet.System.Xml.XPath.XmlDataType where

import Dotnet
import qualified IOExts
import qualified Dotnet.System.Type
import qualified Dotnet.System.Enum

data XmlDataType_ a
type XmlDataType a = Dotnet.System.Enum.Enum (XmlDataType_ a)

data XmlDataTypeTy
 = Text
 | Number
  deriving ( Enum, Show, Read )
toXmlDataType :: XmlDataTypeTy -> XmlDataType ()
toXmlDataType tag = IOExts.unsafePerformIO (Dotnet.System.Enum.parse (IOExts.unsafePerformIO (Dotnet.System.Type.getType "System.Xml.XPath.XmlDataType, System.Xml, Version=1.0.3300.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")) (show tag))

fromXmlDataType :: XmlDataType () -> XmlDataTypeTy
fromXmlDataType obj = IOExts.unsafePerformIO (toString obj >>= return.read)

