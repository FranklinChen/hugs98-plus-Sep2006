module Dotnet.System.Xml where

data XmlNodeType
 = Attribute
 | CDATA
 | Comment
 | Document
 | DocumentFragment
 | Element
 | EndElement
 | EndEntity
 | Entity
 | EntityReference
 | None
 | Notation
 | ProcessingInstruction
 | SignificantWhitespace
 | Text
 | Whitespace
 | XmlDeclaration
   deriving ( Eq, Enum )

data ReadState
 = Closed
 | EndOfFile
 | Error
 | Initial
 | Interactive
   deriving ( Eq, Enum )

data XmlSpace
 = DefaultSpace | NoSpace | PreserveSpace
   deriving ( Eq, Enum )
   
