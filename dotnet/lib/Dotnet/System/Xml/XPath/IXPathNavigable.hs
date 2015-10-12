module Dotnet.System.Xml.XPath.IXPathNavigable where

import Dotnet
import qualified Dotnet.System.Xml.XPath.XPathNavigator
import qualified Dotnet.System.Object

data IXPathNavigable_ a
type IXPathNavigable a = Dotnet.System.Object.Object (IXPathNavigable_ a)

foreign import dotnet
  "method System.Xml.XPath.IXPathNavigable.CreateNavigator"
  createNavigator :: IXPathNavigable obj -> IO (Dotnet.System.Xml.XPath.XPathNavigator.XPathNavigator a0)


