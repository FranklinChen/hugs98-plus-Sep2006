module Dotnet.System.Xml.XmlWriter where

import Dotnet
import qualified Dotnet.System.Object
import Dotnet.System.Xml.XmlReader
import Dotnet.System.Xml.XmlSpace
import Dotnet.System.Xml.WriteState
import qualified Dotnet.System.Array
import qualified Data.Word

data XmlWriter_ a
type XmlWriter a = Dotnet.System.Object.Object (XmlWriter_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteNode"
  writeNode :: Dotnet.System.Xml.XmlReader.XmlReader a0 -> Bool -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteAttributes"
  writeAttributes :: Dotnet.System.Xml.XmlReader.XmlReader a0 -> Bool -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteQualifiedName"
  writeQualifiedName :: String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteName"
  writeName :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteNmToken"
  writeNmToken :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.get_XmlLang"
  get_XmlLang :: XmlWriter obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.get_XmlSpace"
  get_XmlSpace :: XmlWriter obj -> IO (Dotnet.System.Xml.XmlSpace.XmlSpace a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.LookupPrefix"
  lookupPrefix :: String -> XmlWriter obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.Flush"
  flush :: XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.Close"
  close :: XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.get_WriteState"
  get_WriteState :: XmlWriter obj -> IO (Dotnet.System.Xml.WriteState.WriteState a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteBinHex"
  writeBinHex :: Dotnet.System.Array.Array Data.Word.Word8 -> Int -> Int -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteBase64"
  writeBase64 :: Dotnet.System.Array.Array Data.Word.Word8 -> Int -> Int -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteRaw"
  writeRaw :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteRaw"
  writeRaw_1 :: Dotnet.System.Array.Array Char -> Int -> Int -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteChars"
  writeChars :: Dotnet.System.Array.Array Char -> Int -> Int -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteSurrogateCharEntity"
  writeSurrogateCharEntity :: Char -> Char -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteString"
  writeString :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteWhitespace"
  writeWhitespace :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteCharEntity"
  writeCharEntity :: Char -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteEntityRef"
  writeEntityRef :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteProcessingInstruction"
  writeProcessingInstruction :: String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteComment"
  writeComment :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteCData"
  writeCData :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteEndAttribute"
  writeEndAttribute :: XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteStartAttribute"
  writeStartAttribute :: String -> String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteFullEndElement"
  writeFullEndElement :: XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteEndElement"
  writeEndElement :: XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteStartElement"
  writeStartElement :: String -> String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteDocType"
  writeDocType :: String -> String -> String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteEndDocument"
  writeEndDocument :: XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteStartDocument"
  writeStartDocument :: Bool -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteStartDocument"
  writeStartDocument_1 :: XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteStartElement"
  writeStartElement_1 :: String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteStartElement"
  writeStartElement_2 :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteAttributeString"
  writeAttributeString :: String -> String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteAttributeString"
  writeAttributeString_1 :: String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteAttributeString"
  writeAttributeString_2 :: String -> String -> String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteStartAttribute"
  writeStartAttribute_1 :: String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteElementString"
  writeElementString :: String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWriter.WriteElementString"
  writeElementString_1 :: String -> String -> String -> XmlWriter obj -> IO (())


