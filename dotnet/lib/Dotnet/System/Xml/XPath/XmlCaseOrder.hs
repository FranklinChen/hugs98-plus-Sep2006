module Dotnet.System.Xml.XPath.XmlCaseOrder where

import Dotnet
import qualified IOExts
import qualified Dotnet.System.Type
import qualified Dotnet.System.Enum

data XmlCaseOrder_ a
type XmlCaseOrder a = Dotnet.System.Enum.Enum (XmlCaseOrder_ a)

data XmlCaseOrderTy
 = None
 | UpperFirst
 | LowerFirst
  deriving ( Enum, Show, Read )
toXmlCaseOrder :: XmlCaseOrderTy -> XmlCaseOrder ()
toXmlCaseOrder tag = IOExts.unsafePerformIO (Dotnet.System.Enum.parse (IOExts.unsafePerformIO (Dotnet.System.Type.getType "System.Xml.XPath.XmlCaseOrder, System.Xml, Version=1.0.3300.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")) (show tag))

fromXmlCaseOrder :: XmlCaseOrder () -> XmlCaseOrderTy
fromXmlCaseOrder obj = IOExts.unsafePerformIO (toString obj >>= return.read)

