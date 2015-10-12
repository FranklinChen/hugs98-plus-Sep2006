-- print the current directory
module Main where

import System.Directory

main = getCurrentDirectory >>= putStrLn
