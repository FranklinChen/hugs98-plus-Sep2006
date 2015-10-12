module Dotnet.System.Xml.XmlImplementation where

import Dotnet
import qualified Dotnet.System.Object
import Dotnet.System.Xml.XmlDocumentTy

data XmlImplementation_ a
type XmlImplementation a = Dotnet.System.Object.Object (XmlImplementation_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlImplementation.CreateDocument"
  createDocument :: XmlImplementation obj -> IO (Dotnet.System.Xml.XmlDocumentTy.XmlDocument a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlImplementation.HasFeature"
  hasFeature :: String -> String -> XmlImplementation obj -> IO (Bool)


