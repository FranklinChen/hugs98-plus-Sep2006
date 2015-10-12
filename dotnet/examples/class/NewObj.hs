--
-- Demonstrating how to dynamically create a new .NET class
-- containing methods that call back into Haskell.
--
module NewObj where

import Dotnet
import List

{- from DotNet; for reference:
data Class 
 = Class String		-- type/class name
 	 (Maybe String) -- Just x => derive from x
 	 [Method]

data Method
 = Method String       -- .NET name
	  Bool         -- True => override.
 	  String       -- Haskell function to call
	  [Type]       -- Argument types
	  (Maybe Type) -- result (Nothing => void).

-- for now, let's be modest..
data Type = Object | String | Int
-}

greeting :: Object () -> Object b -> IO ()
greeting o _ = do
  x <- (result o) >>= hsValue
  msgBox "hugs98" ("Hugs98.NET here " ++ x)

foreign import dotnet
  "method Greeting"
  greet :: Object a -> Object b -> Object c -> IO ()

newObject :: IO ()
newObject = do
  let newType
       = Class "HugsObject"
       	       Nothing
       	       [Method "Greeting" False
	       	       "NewObj.greeting"
		       [ObjectTy Nothing,ObjectTy Nothing]
		       Nothing]
  obj <- defineClass newType
  print (obj :: Object ())
  obj # greet obj obj
  putStrLn "done"
