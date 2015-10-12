--
-- SmtpMail.Send example
--
module Mail where

import Dotnet

foreign import dotnet
  "static System.Web.Mail.SmtpMail.Send"
  sendMail :: String  -- fromAddr
  	   -> String  -- toAddr
	   -> String  -- subject
	   -> String  -- body
	   -> IO ()

test from toA = do
 sendMail from toA 
 	  "Hugs98.net test"
	  "<html><body>Greetings from <em>Hugs98.NET</em></body></html>"
