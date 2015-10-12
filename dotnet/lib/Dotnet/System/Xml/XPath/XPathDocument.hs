module Dotnet.System.Xml.XPath.XPathDocument where

import Dotnet
import qualified Dotnet.System.Xml.XPath.XPathNavigator
import qualified Dotnet.System.Object

data XPathDocument_ a
type XPathDocument a = Dotnet.System.Object.Object (XPathDocument_ a)

foreign import dotnet
  "method System.Xml.XPath.XPathDocument.CreateNavigator"
  createNavigator :: XPathDocument obj -> IO (Dotnet.System.Xml.XPath.XPathNavigator.XPathNavigator a0)


