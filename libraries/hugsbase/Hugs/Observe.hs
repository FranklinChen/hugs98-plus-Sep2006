module Hugs.Observe (observe, bkpt, setBkpt) where

primitive observe :: String -> a -> a
primitive bkpt    :: String -> a -> a
primitive setBkpt :: String -> Bool -> IO ()
