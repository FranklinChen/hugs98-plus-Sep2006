module Dotnet.System.Web.Mail.MailAttachment where

import Dotnet
import qualified Dotnet.System.Object
import qualified Dotnet.System.Web.Mail.MailEncoding

data MailAttachment_ a
type MailAttachment a = Dotnet.System.Object.Object (MailAttachment_ a)

foreign import dotnet
  "method System.Web.Mail.MailAttachment.get_Filename"
  get_Filename :: MailAttachment obj -> IO (String)

foreign import dotnet
  "method System.Web.Mail.MailAttachment.get_Encoding"
  get_Encoding :: MailAttachment obj -> IO (Dotnet.System.Web.Mail.MailEncoding.MailEncoding a0)


