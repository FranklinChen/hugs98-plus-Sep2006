module Dotnet.System.Console where

import Dotnet.System.ObjectTy
import Char

data Console_ a
type Console a = Object (Console_ a)

foreign import dotnet
  "static Dotnet.System.Console.Read"
  readChar :: IO Int

foreign import dotnet
  "static Dotnet.System.Console.ReadLine"
  readLine :: IO String

foreign import dotnet
  "static Dotnet.System.Console.Write"
  writeChar :: Char -> IO ()

foreign import dotnet
  "static Dotnet.System.Console.WriteLine"
  writeLine :: String -> IO ()


