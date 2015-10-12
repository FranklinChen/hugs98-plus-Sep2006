module Main where

import List (isPrefixOf)
import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Types
import Text.PrettyPrint.HughesPJ (render)
import Text.XML.HaXml.Pretty     (document)

-- Test stuff
--value1 :: ([(Bool,Int)],(String,Maybe Char))
value1 = True

--main = do (putStrLn . render . document . toXml) value2

main = fWriteXml "/dev/tty" value1
        
