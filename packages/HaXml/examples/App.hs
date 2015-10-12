module Main where

import System (getArgs)
import IO

import Text.XML.HaXml.Wrappers (fix2Args)
import Text.XML.HaXml.XmlContent (fReadXml, fWriteXml)
import AlbumDTD

main =
  fix2Args >>= \(infile,outfile)->
  do putStrLn ("reading "++infile)
     value <- fReadXml infile
     putStrLn ("checking value's type and album title")
     putStrLn (let (Album title _ _ _ _ _ _ _) = value in
               if title==(Title "Time Out") then "ok" else "failed")
     putStrLn ("writing "++outfile)
     v <- (let (Album _ b c d e f g h) = value in
           return (Album (Title "unknown") b c d e f g h))
     fWriteXml outfile v
     putStrLn ("Done.")

