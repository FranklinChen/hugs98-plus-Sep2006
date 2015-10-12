module Dotnet.System.Xml.XmlCharacterData where

import Dotnet
import qualified Dotnet.System.Xml.XmlLinkedNode

data XmlCharacterData_ a
type XmlCharacterData a = Dotnet.System.Xml.XmlLinkedNode.XmlLinkedNode (XmlCharacterData_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlCharacterData.ReplaceData"
  replaceData :: Int -> Int -> String -> XmlCharacterData obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlCharacterData.DeleteData"
  deleteData :: Int -> Int -> XmlCharacterData obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlCharacterData.InsertData"
  insertData :: Int -> String -> XmlCharacterData obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlCharacterData.AppendData"
  appendData :: String -> XmlCharacterData obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlCharacterData.Substring"
  substring :: Int -> Int -> XmlCharacterData obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlCharacterData.get_Length"
  get_Length :: XmlCharacterData obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Xml.XmlCharacterData.set_Data"
  set_Data :: String -> XmlCharacterData obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlCharacterData.get_Data"
  get_Data :: XmlCharacterData obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlCharacterData.set_InnerText"
  set_InnerText :: String -> XmlCharacterData obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlCharacterData.get_InnerText"
  get_InnerText :: XmlCharacterData obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlCharacterData.set_Value"
  set_Value :: String -> XmlCharacterData obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlCharacterData.get_Value"
  get_Value :: XmlCharacterData obj -> IO (String)


