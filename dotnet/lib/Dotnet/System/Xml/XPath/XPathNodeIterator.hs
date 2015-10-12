module Dotnet.System.Xml.XPath.XPathNodeIterator where

import Dotnet
import qualified Dotnet.System.Xml.XPath.XPathNavigatorTy
import qualified Dotnet.System.Object

data XPathNodeIterator_ a
type XPathNodeIterator a = Dotnet.System.Object.Object (XPathNodeIterator_ a)

foreign import dotnet
  "method System.Xml.XPath.XPathNodeIterator.get_Count"
  get_Count :: XPathNodeIterator obj -> IO (Int)

foreign import dotnet
  "method System.Xml.XPath.XPathNodeIterator.get_CurrentPosition"
  get_CurrentPosition :: XPathNodeIterator obj -> IO (Int)

foreign import dotnet
  "method System.Xml.XPath.XPathNodeIterator.get_Current"
  get_Current :: XPathNodeIterator obj -> IO (Dotnet.System.Xml.XPath.XPathNavigatorTy.XPathNavigator a0)

foreign import dotnet
  "method System.Xml.XPath.XPathNodeIterator.MoveNext"
  moveNext :: XPathNodeIterator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNodeIterator.Clone"
  clone :: XPathNodeIterator obj -> IO (Dotnet.System.Xml.XPath.XPathNodeIterator.XPathNodeIterator a0)


