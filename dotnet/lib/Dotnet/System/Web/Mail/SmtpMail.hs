module Dotnet.System.Web.Mail.SmtpMail where

import Dotnet
import qualified Dotnet.System.Object
import qualified Dotnet.System.Web.Mail.MailMessage

data SmtpMail_ a
type SmtpMail a = Dotnet.System.Object.Object (SmtpMail_ a)

foreign import dotnet
  "static method System.Web.Mail.SmtpMail.get_SmtpServer"
  get_SmtpServer :: IO (String)

foreign import dotnet
  "static method System.Web.Mail.SmtpMail.set_SmtpServer"
  set_SmtpServer :: String -> IO (())

foreign import dotnet
  "static method System.Web.Mail.SmtpMail.Send"
  send :: String -> String -> String -> String -> IO (())

foreign import dotnet
  "static method System.Web.Mail.SmtpMail.Send"
  send_1 :: Dotnet.System.Web.Mail.MailMessage.MailMessage a0 -> IO (())


