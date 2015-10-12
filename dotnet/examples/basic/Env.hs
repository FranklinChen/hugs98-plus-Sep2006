--
-- Calling a static method
--
module Env where

import Dotnet

foreign import dotnet
  "static System.Environment.GetEnvironmentVariable"
  getEnv :: String -> IO String

test :: IO ()
test = do
  let var = "COMSPEC"
  s <- getEnv var
  putStrLn (var ++ " = " ++ s)
