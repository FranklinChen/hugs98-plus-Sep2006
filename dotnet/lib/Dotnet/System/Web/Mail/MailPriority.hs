module Dotnet.System.Web.Mail.MailPriority where

import Dotnet
import qualified Dotnet.System.Enum
import qualified IOExts
import qualified Dotnet.System.Type
import qualified Dotnet.System.Enum

data MailPriority_ a
type MailPriority a = Dotnet.System.Enum.Enum (MailPriority_ a)

data MailPriorityTy
 = Normal
 | Low
 | High
  deriving ( Enum, Show, Read )
toMailPriority :: MailPriorityTy -> MailPriority ()
toMailPriority tag = IOExts.unsafePerformIO (Dotnet.System.Enum.parse (IOExts.unsafePerformIO (Dotnet.System.Type.getType "System.Web.Mail.MailPriority, System.Web, Version=1.0.3300.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a")) (show tag))

fromMailPriority :: MailPriority () -> MailPriorityTy
fromMailPriority obj = IOExts.unsafePerformIO (toString obj >>= return.read)

