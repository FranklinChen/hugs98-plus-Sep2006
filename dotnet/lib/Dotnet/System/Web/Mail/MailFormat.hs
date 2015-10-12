module Dotnet.System.Web.Mail.MailFormat where

import Dotnet
import qualified Dotnet.System.Enum
import qualified IOExts
import qualified Dotnet.System.Type
import qualified Dotnet.System.Enum

data MailFormat_ a
type MailFormat a = Dotnet.System.Enum.Enum (MailFormat_ a)

data MailFormatTy
 = Text
 | Html
  deriving ( Enum, Show, Read )
toMailFormat :: MailFormatTy -> MailFormat ()
toMailFormat tag = IOExts.unsafePerformIO (Dotnet.System.Enum.parse (IOExts.unsafePerformIO (Dotnet.System.Type.getType "System.Web.Mail.MailFormat, System.Web, Version=1.0.3300.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a")) (show tag))

fromMailFormat :: MailFormat () -> MailFormatTy
fromMailFormat obj = IOExts.unsafePerformIO (toString obj >>= return.read)

