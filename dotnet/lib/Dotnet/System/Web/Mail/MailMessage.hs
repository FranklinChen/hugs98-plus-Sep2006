module Dotnet.System.Web.Mail.MailMessage where

import Dotnet
import qualified Dotnet.System.Object
import qualified Dotnet.System.Web.Mail.MailPriority
import qualified Dotnet.System.Web.Mail.MailFormat
import qualified Dotnet.System.Text.Encoding
import qualified Dotnet.System.Collections.IDictionary
import qualified Dotnet.System.Collections.IList

data MailMessage_ a
type MailMessage a = Dotnet.System.Object.Object (MailMessage_ a)

foreign import dotnet
  "method System.Web.Mail.MailMessage.get_From"
  get_From :: MailMessage obj -> IO (String)

foreign import dotnet
  "method System.Web.Mail.MailMessage.set_From"
  set_From :: String -> MailMessage obj -> IO (())

foreign import dotnet
  "method System.Web.Mail.MailMessage.get_To"
  get_To :: MailMessage obj -> IO (String)

foreign import dotnet
  "method System.Web.Mail.MailMessage.set_To"
  set_To :: String -> MailMessage obj -> IO (())

foreign import dotnet
  "method System.Web.Mail.MailMessage.get_Cc"
  get_Cc :: MailMessage obj -> IO (String)

foreign import dotnet
  "method System.Web.Mail.MailMessage.set_Cc"
  set_Cc :: String -> MailMessage obj -> IO (())

foreign import dotnet
  "method System.Web.Mail.MailMessage.get_Bcc"
  get_Bcc :: MailMessage obj -> IO (String)

foreign import dotnet
  "method System.Web.Mail.MailMessage.set_Bcc"
  set_Bcc :: String -> MailMessage obj -> IO (())

foreign import dotnet
  "method System.Web.Mail.MailMessage.get_Subject"
  get_Subject :: MailMessage obj -> IO (String)

foreign import dotnet
  "method System.Web.Mail.MailMessage.set_Subject"
  set_Subject :: String -> MailMessage obj -> IO (())

foreign import dotnet
  "method System.Web.Mail.MailMessage.get_Priority"
  get_Priority :: MailMessage obj -> IO (Dotnet.System.Web.Mail.MailPriority.MailPriority a0)

foreign import dotnet
  "method System.Web.Mail.MailMessage.set_Priority"
  set_Priority :: Dotnet.System.Web.Mail.MailPriority.MailPriority a0 -> MailMessage obj -> IO (())

foreign import dotnet
  "method System.Web.Mail.MailMessage.get_UrlContentBase"
  get_UrlContentBase :: MailMessage obj -> IO (String)

foreign import dotnet
  "method System.Web.Mail.MailMessage.set_UrlContentBase"
  set_UrlContentBase :: String -> MailMessage obj -> IO (())

foreign import dotnet
  "method System.Web.Mail.MailMessage.get_UrlContentLocation"
  get_UrlContentLocation :: MailMessage obj -> IO (String)

foreign import dotnet
  "method System.Web.Mail.MailMessage.set_UrlContentLocation"
  set_UrlContentLocation :: String -> MailMessage obj -> IO (())

foreign import dotnet
  "method System.Web.Mail.MailMessage.get_Body"
  get_Body :: MailMessage obj -> IO (String)

foreign import dotnet
  "method System.Web.Mail.MailMessage.set_Body"
  set_Body :: String -> MailMessage obj -> IO (())

foreign import dotnet
  "method System.Web.Mail.MailMessage.get_BodyFormat"
  get_BodyFormat :: MailMessage obj -> IO (Dotnet.System.Web.Mail.MailFormat.MailFormat a0)

foreign import dotnet
  "method System.Web.Mail.MailMessage.set_BodyFormat"
  set_BodyFormat :: Dotnet.System.Web.Mail.MailFormat.MailFormat a0 -> MailMessage obj -> IO (())

foreign import dotnet
  "method System.Web.Mail.MailMessage.get_BodyEncoding"
  get_BodyEncoding :: MailMessage obj -> IO (Dotnet.System.Text.Encoding.Encoding a0)

foreign import dotnet
  "method System.Web.Mail.MailMessage.set_BodyEncoding"
  set_BodyEncoding :: Dotnet.System.Text.Encoding.Encoding a0 -> MailMessage obj -> IO (())

foreign import dotnet
  "method System.Web.Mail.MailMessage.get_Headers"
  get_Headers :: MailMessage obj -> IO (Dotnet.System.Collections.IDictionary.IDictionary a0)

foreign import dotnet
  "method System.Web.Mail.MailMessage.get_Attachments"
  get_Attachments :: MailMessage obj -> IO (Dotnet.System.Collections.IList.IList a0)


