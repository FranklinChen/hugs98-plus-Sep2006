module Dotnet.System.Xml.XmlDocument 
	( module Dotnet.System.Xml.XmlDocument ,
	  module Dotnet.System.Xml.XmlDocumentTy
	) where

import Dotnet
import qualified Dotnet.System.Xml.XmlNode
import qualified Dotnet.System.Xml.XmlDocumentTy
import Dotnet.System.Xml.XmlWriter
import Dotnet.System.IO.TextWriter
import Dotnet.System.IO.Stream
import Dotnet.System.Xml.XmlReader
import Dotnet.System.IO.TextReader
import Dotnet.System.Xml.XmlNodeTy
import Dotnet.System.Xml.XmlNodeType
import Dotnet.System.Xml.XmlElementTy
import Dotnet.System.Xml.XmlAttributeTy
import Dotnet.System.Xml.XmlNodeList
import Dotnet.System.Xml.XmlWhitespace
import Dotnet.System.Xml.XmlSignificantWhitespace
import Dotnet.System.Xml.XmlText
import Dotnet.System.Xml.XmlDeclaration
import Dotnet.System.Xml.XmlProcessingInstruction
import Dotnet.System.Xml.XmlEntityReference
import Dotnet.System.Xml.XmlDocumentFragment
import Dotnet.System.Xml.XmlDocumentType
import Dotnet.System.Xml.XmlComment
import Dotnet.System.Xml.XmlCDataSection
import Dotnet.System.Xml.XmlResolver
import Dotnet.System.Xml.XmlImplementation
import Dotnet.System.Xml.XmlNameTable
--import Dotnet.System.Xml.XmlNodeChangedEventHandler

data XmlDocument_ a
type XmlDocument a = Dotnet.System.Xml.XmlNodeTy.XmlNode (XmlDocument_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.Save"
  save :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.Save"
  save_1 :: Dotnet.System.IO.TextWriter.TextWriter a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.Save"
  save_2 :: Dotnet.System.IO.Stream.Stream a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.Save"
  save_3 :: String -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.LoadXml"
  loadXml :: String -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.Load"
  load :: Dotnet.System.Xml.XmlReader.XmlReader a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.Load"
  load_1 :: Dotnet.System.IO.TextReader.TextReader a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.Load"
  load_2 :: Dotnet.System.IO.Stream.Stream a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.Load"
  load_3 :: String -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.ReadNode"
  readNode :: Dotnet.System.Xml.XmlReader.XmlReader a0 -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateNode"
  createNode :: Dotnet.System.Xml.XmlNodeType.XmlNodeType a0 -> String -> String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a3)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateNode"
  createNode_1 :: String -> String -> String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a3)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateNode"
  createNode_2 :: Dotnet.System.Xml.XmlNodeType.XmlNodeType a0 -> String -> String -> String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a4)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateElement"
  createElement :: String -> String -> String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlElementTy.XmlElement a3)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateAttribute"
  createAttribute :: String -> String -> String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a3)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.ImportNode"
  importNode :: Dotnet.System.Xml.XmlNodeTy.XmlNode a0 -> Bool -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.GetElementById"
  getElementById :: String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlElementTy.XmlElement a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.GetElementsByTagName"
  getElementsByTagName :: String -> String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlNodeList.XmlNodeList a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.GetElementsByTagName"
  getElementsByTagName_1 :: String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlNodeList.XmlNodeList a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateWhitespace"
  createWhitespace :: String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlWhitespace.XmlWhitespace a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateSignificantWhitespace"
  createSignificantWhitespace :: String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlSignificantWhitespace.XmlSignificantWhitespace a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateTextNode"
  createTextNode :: String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlText.XmlText a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateXmlDeclaration"
  createXmlDeclaration :: String -> String -> String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlDeclaration.XmlDeclaration a3)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateProcessingInstruction"
  createProcessingInstruction :: String -> String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlProcessingInstruction.XmlProcessingInstruction a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateEntityReference"
  createEntityReference :: String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlEntityReference.XmlEntityReference a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateDocumentFragment"
  createDocumentFragment :: XmlDocument obj -> IO (Dotnet.System.Xml.XmlDocumentFragment.XmlDocumentFragment a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateDocumentType"
  createDocumentType :: String -> String -> String -> String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlDocumentType.XmlDocumentType a4)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateComment"
  createComment :: String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlComment.XmlComment a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateCDataSection"
  createCDataSection :: String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlCDataSection.XmlCDataSection a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.set_XmlResolver"
  set_XmlResolver :: Dotnet.System.Xml.XmlResolver.XmlResolver a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.get_DocumentType"
  get_DocumentType :: XmlDocument obj -> IO (Dotnet.System.Xml.XmlDocumentType.XmlDocumentType a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.WriteContentTo"
  writeContentTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.WriteTo"
  writeTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.get_BaseURI"
  get_BaseURI :: XmlDocument obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.set_InnerXml"
  set_InnerXml :: String -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.get_InnerXml"
  get_InnerXml :: XmlDocument obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.get_IsReadOnly"
  get_IsReadOnly :: XmlDocument obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.get_LocalName"
  get_LocalName :: XmlDocument obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CloneNode"
  cloneNode :: Bool -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.get_OwnerDocument"
  get_OwnerDocument :: XmlDocument obj -> IO (Dotnet.System.Xml.XmlDocument.XmlDocument a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.get_NodeType"
  get_NodeType :: XmlDocument obj -> IO (Dotnet.System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.get_Name"
  get_Name :: XmlDocument obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.get_Implementation"
  get_Implementation :: XmlDocument obj -> IO (Dotnet.System.Xml.XmlImplementation.XmlImplementation a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.get_DocumentElement"
  get_DocumentElement :: XmlDocument obj -> IO (Dotnet.System.Xml.XmlElementTy.XmlElement a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateAttribute"
  createAttribute_1 :: String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateElement"
  createElement_1 :: String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlElementTy.XmlElement a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateAttribute"
  createAttribute_2 :: String -> String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.CreateElement"
  createElement_2 :: String -> String -> XmlDocument obj -> IO (Dotnet.System.Xml.XmlElementTy.XmlElement a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.get_NameTable"
  get_NameTable :: XmlDocument obj -> IO (Dotnet.System.Xml.XmlNameTable.XmlNameTable a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.get_PreserveWhitespace"
  get_PreserveWhitespace :: XmlDocument obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.set_PreserveWhitespace"
  set_PreserveWhitespace :: Bool -> XmlDocument obj -> IO (())

{-
foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.add_NodeInserting"
  add_NodeInserting :: Dotnet.System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.remove_NodeInserting"
  remove_NodeInserting :: Dotnet.System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.add_NodeInserted"
  add_NodeInserted :: Dotnet.System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.remove_NodeInserted"
  remove_NodeInserted :: Dotnet.System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.add_NodeRemoving"
  add_NodeRemoving :: Dotnet.System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.remove_NodeRemoving"
  remove_NodeRemoving :: Dotnet.System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.add_NodeRemoved"
  add_NodeRemoved :: Dotnet.System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.remove_NodeRemoved"
  remove_NodeRemoved :: Dotnet.System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.add_NodeChanging"
  add_NodeChanging :: Dotnet.System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.remove_NodeChanging"
  remove_NodeChanging :: Dotnet.System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.add_NodeChanged"
  add_NodeChanged :: Dotnet.System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocument.remove_NodeChanged"
  remove_NodeChanged :: Dotnet.System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())


-}
