--
-- Haskell wrapper for Dotnet.System.Xml.XmlReader
-- 
module Dotnet.System.Xml.XmlReader where

import Dotnet
import qualified Dotnet.System.Object
import Dotnet.System.Xml.XmlNameTable
import Dotnet.System.Xml

data XmlReader_ a 
type XmlReader a = Dotnet.System.Object.Object (XmlReader_ a)

attributeCount :: XmlReader a -> IO Int
attributeCount = invoke "get_AttributeCount" ()

baseURI :: XmlReader a -> IO String
baseURI = invoke "get_BaseURI" ()

canResolveEntity :: XmlReader a -> IO Bool
canResolveEntity = invoke "get_CanResolveEntity" ()

depth :: XmlReader a -> IO Int
depth = invoke "get_Depth" ()

eof :: XmlReader a -> IO Bool
eof = invoke "get_EOF" ()

hasAttributes :: XmlReader a -> IO Bool
hasAttributes = invoke "get_HasAttributes" ()

hasValue :: XmlReader a -> IO Bool
hasValue = invoke "get_HasValue" ()

isDefault :: XmlReader a -> IO Bool
isDefault = invoke "get_IsDefault" ()

isEmptyElement :: XmlReader a -> IO Bool
isEmptyElement = invoke "get_IsEmptyElement" ()

itemIndex :: Int -> XmlReader a -> IO String
itemIndex idx = invoke "get_Item" idx

itemName :: String -> XmlReader a -> IO String
itemName nm = invoke "get_Item" nm

itemNameURI :: String -> String -> XmlReader a -> IO String
itemNameURI nm uri = invoke "get_Item" (nm,uri)

localName :: XmlReader a -> IO String
localName = invoke "get_LocalName" ()

name :: XmlReader a -> IO String
name = invoke "get_Name" ()

namespaceURI :: XmlReader a -> IO String
namespaceURI = invoke "get_NamespaceURI" ()

nameTable :: XmlReader a -> IO (Dotnet.System.Xml.XmlNameTable.XmlNameTable b)
nameTable = invoke "get_NameTable" ()

nodeType :: XmlReader a -> IO Dotnet.System.Xml.XmlNodeType
nodeType this = do
  v <- this # invoke "get_NodeType" ()
  return (toEnum v)

prefix :: XmlReader a -> IO String
prefix = invoke "get_Prefix" ()

quoteChar :: XmlReader a -> IO Char
quoteChar = invoke "get_QuoteChar" ()

readState :: XmlReader a -> IO Dotnet.System.Xml.ReadState
readState this = do
  v <- this # invoke "get_ReadState" ()
  return (toEnum v)

value :: XmlReader a -> IO String
value = invoke "get_Value" ()

xmlLang :: XmlReader a -> IO String
xmlLang = invoke "get_XmlLang" ()

xmlSpace :: XmlReader a -> IO Dotnet.System.Xml.XmlSpace
xmlSpace this = do
  v <- this # invoke "get_XmlSpace" ()
  return (toEnum v)

close :: XmlReader a -> IO ()
close = invoke "Close" ()

getAttributeIndex :: Int -> XmlReader a -> IO String
getAttributeIndex idx = invoke "GetAttribute" idx

getAttributeName :: String -> XmlReader a -> IO String
getAttributeName nm = invoke "GetAttribute" nm

getAttributeNameURI :: String -> String -> XmlReader a -> IO String
getAttributeNameURI nm uri = invoke "getAttribute" (nm,uri)

isName :: String -> XmlReader a -> IO Bool
isName str = invoke "IsName" str

isNameToken :: String -> XmlReader a -> IO Bool
isNameToken str = invoke "IsNameToken" str

isStartElement :: XmlReader a -> IO Bool
isStartElement = invoke "IsStartElement" ()

isStartElementName :: String -> XmlReader a -> IO Bool
isStartElementName str = invoke "IsStartElement" str

isStartElementNameURI :: String -> String -> XmlReader a -> IO Bool
isStartElementNameURI str uri = invoke "IsStartElement" (str,uri)

lookupNamespace :: String -> XmlReader a -> IO String
lookupNamespace str = invoke "LookupNamespace" str

moveToAttributeIndex :: Int -> XmlReader a -> IO ()
moveToAttributeIndex idx = invoke "MoveToAttribute" idx

moveToAttributeName :: String -> XmlReader a -> IO Bool
moveToAttributeName str = invoke "MoveToAttribute" str

moveToAttributeNameURI :: String -> String -> XmlReader a -> IO Bool
moveToAttributeNameURI str uri = invoke "MoveToAttribute" (str,uri)

moveToContent :: XmlReader a -> IO Dotnet.System.Xml.XmlNodeType
moveToContent this = do
  v <- this # invoke "MoveToContent" ()
  return (toEnum v)

moveToElement :: XmlReader a -> IO Bool
moveToElement = invoke "MoveToElement" ()

moveToFirstAttribute :: XmlReader a -> IO Bool
moveToFirstAttribute = invoke "MoveToFirstAttribute" ()

moveToNextAttribute :: XmlReader a -> IO Bool
moveToNextAttribute = invoke "MoveToNextAttribute" ()

readNext :: XmlReader a -> IO Bool
readNext = invoke "Read" ()

readAttributeValue :: XmlReader a -> IO Bool
readAttributeValue = invoke "ReadAttributeValue" ()

readElementString :: XmlReader a -> IO String
readElementString = invoke "ReadElementString" ()

readElementStringName :: String -> XmlReader a -> IO String
readElementStringName str = invoke "ReadElementString" str

readElementStringNameURI :: String -> String -> XmlReader a -> IO String
readElementStringNameURI str uri = invoke "ReadElementString" (str,uri)

readEndElement :: XmlReader a -> IO ()
readEndElement = invoke "ReadEndElement" ()

readInnerXml :: XmlReader a -> IO String
readInnerXml = invoke "ReadInnerXml" ()

readOuterXml :: XmlReader a -> IO String
readOuterXml = invoke "ReadOuterXml" ()

readStartElement :: XmlReader a -> IO ()
readStartElement = invoke "ReadStartElement" ()

readStartElementName :: String -> XmlReader a -> IO ()
readStartElementName str = invoke "ReadStartElement" str

readStartElementNameURI :: String -> String -> XmlReader a -> IO ()
readStartElementNameURI str uri = invoke "ReadStartElement" (str,uri)

readString :: XmlReader a -> IO String
readString = invoke "ReadString" ()

resolveEntity :: XmlReader a -> IO ()
resolveEntity = invoke "ResolveEntity" ()

skip :: XmlReader a -> IO ()
skip = invoke "Skip" ()







