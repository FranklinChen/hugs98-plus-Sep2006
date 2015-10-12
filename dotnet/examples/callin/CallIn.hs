--
-- Demonstrating explicit calling in to Haskell from
-- .NET code (directly via the Hugs Server API).
--
-- The external code calling in can be found in print.cs 
--
module CallIn where

import Dotnet

foreign import dotnet
  "static [print.dll]Print.p"
  printIt :: Object () -> IO ()

callIn :: IO ()
callIn = do
    -- mildly bogus to create Hugs.Server object, since
    -- its methods are all static..
  serv <- new "Hugs.Server"
  print serv
  printIt serv
--  (invokeStatic "[print.dll]Print" "p" serv) :: IO (Object Char)
  putStrLn "done"

-- the entry point that will be called from Print.p()
greeting = putStrLn "In Haskell: Greetings from Hugs98.NET"
