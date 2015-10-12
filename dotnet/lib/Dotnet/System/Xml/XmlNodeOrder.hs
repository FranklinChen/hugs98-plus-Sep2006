module Dotnet.System.Xml.XmlNodeOrder where

import Dotnet
import qualified IOExts
import qualified Dotnet.System.Type
import qualified Dotnet.System.Enum

data XmlNodeOrder_ a
type XmlNodeOrder a = Dotnet.System.Enum.Enum (XmlNodeOrder_ a)

data XmlNodeOrderTy
 = Before
 | After
 | Same
 | Unknown
  deriving ( Enum, Show, Read )
toXmlNodeOrder :: XmlNodeOrderTy -> XmlNodeOrder ()
toXmlNodeOrder tag = IOExts.unsafePerformIO (Dotnet.System.Enum.parse (IOExts.unsafePerformIO (Dotnet.System.Type.getType "System.Xml.XmlNodeOrder, System.Xml, Version=1.0.3300.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")) (show tag))

fromXmlNodeOrder :: XmlNodeOrder () -> XmlNodeOrderTy
fromXmlNodeOrder obj = IOExts.unsafePerformIO (toString obj >>= return.read)

