module Dotnet.System.Web.Mail.MailEncoding where

import Dotnet
import qualified IOExts
import qualified Dotnet.System.Type
import qualified Dotnet.System.Enum

data MailEncoding_ a
type MailEncoding a = Dotnet.System.Enum.Enum (MailEncoding_ a)

data MailEncodingTy
 = UUEncode
 | Base64
  deriving ( Enum, Show, Read )
toMailEncoding :: MailEncodingTy -> MailEncoding ()
toMailEncoding tag = IOExts.unsafePerformIO (Dotnet.System.Enum.parse (IOExts.unsafePerformIO (Dotnet.System.Type.getType "System.Web.Mail.MailEncoding, System.Web, Version=1.0.3300.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a")) (show tag))

fromMailEncoding :: MailEncoding () -> MailEncodingTy
fromMailEncoding obj = IOExts.unsafePerformIO (toString obj >>= return.read)

