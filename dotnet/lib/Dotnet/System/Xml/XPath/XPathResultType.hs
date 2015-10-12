module Dotnet.System.Xml.XPath.XPathResultType where

import Dotnet
import qualified IOExts
import qualified Dotnet.System.Type
import qualified Dotnet.System.Enum

data XPathResultType_ a
type XPathResultType a = Dotnet.System.Enum.Enum (XPathResultType_ a)

data XPathResultTypeTy
 = Number
 | String
 | Boolean
 | NodeSet
 | Navigator
 | Any
 | Error
  deriving ( Enum, Show, Read )
toXPathResultType :: XPathResultTypeTy -> XPathResultType ()
toXPathResultType tag = IOExts.unsafePerformIO (Dotnet.System.Enum.parse (IOExts.unsafePerformIO (Dotnet.System.Type.getType "System.Xml.XPath.XPathResultType, System.Xml, Version=1.0.3300.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")) (show tag))

fromXPathResultType :: XPathResultType () -> XPathResultTypeTy
fromXPathResultType obj = IOExts.unsafePerformIO (toString obj >>= return.read)

