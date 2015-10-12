module Main where

import Xml2Haskell
import Geisler  -- generated with DtdToHaskell from Geisler.dtd

readFoo :: IO Foo
readFoo = readXml "-"

writeFoo :: Foo -> IO ()
writeFoo = writeXml "-"

main = do
  foo <- readFoo
  writeFoo foo
