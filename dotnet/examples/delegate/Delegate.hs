--
-- Packaging up Haskell function values as delegates.
--
-- The UI which invokes the delegate can be found in ui.cs
-- 
-- Note: you have to start 'hugs' from within its
-- installation to have this example work.
--
module Delegate where

import Dotnet

foreign import dotnet
  "static System.Windows.Forms.Application.Run"
  run :: Object a -> IO ()

delegate :: IO ()
delegate = do
  ui      <- new "[ui.dll]UITest"
  upHandler   <- newDelegator ( \ _ _ -> ui # invoke "IncCount" ())
  downHandler <- newDelegator ( \ _ _ -> ui # invoke "DecCount" ())
--msgBox "Hugs98.NET" "a *real* delegate :-)")
  ()      <- ui # invoke "AddHandlerUp" upHandler
  ()      <- ui # invoke "AddHandlerDown" downHandler
-- ToDo: the InvokeBridge is unable to locate this static method; look into.
--       For now, use .NET stub method do it for us.
--  run ui
  ()      <- ui # invoke "RunIt" ()
  putStrLn "done"
