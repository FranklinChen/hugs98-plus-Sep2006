module Dotnet.System.Xml.XmlNodeType where

import Dotnet
import qualified Dotnet.System.Enum
import qualified IOExts
import qualified Dotnet.System.Type
import qualified Dotnet.System.Enum

data XmlNodeType_ a
type XmlNodeType a = Dotnet.System.Enum.Enum (XmlNodeType_ a)

data XmlNodeTypeTy
 = Value__
 | None
 | Element
 | Attribute
 | Text
 | CDATA
 | EntityReference
 | Entity
 | ProcessingInstruction
 | Comment
 | Document
 | DocumentType
 | DocumentFragment
 | Notation
 | Whitespace
 | SignificantWhitespace
 | EndElement
 | EndEntity
 | XmlDeclaration
  deriving ( Enum, Show, Read )
toXmlNodeType :: XmlNodeTypeTy -> XmlNodeType ()
toXmlNodeType tag = IOExts.unsafePerformIO (Dotnet.System.Enum.parse (IOExts.unsafePerformIO (Dotnet.System.Type.getType "System.Xml.XmlNodeType, Dotnet.System.Xml, Version=1.0.3300.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")) (show tag))

fromXmlNodeType :: XmlNodeType () -> XmlNodeTypeTy
fromXmlNodeType obj = IOExts.unsafePerformIO (toString obj >>= return.read)

