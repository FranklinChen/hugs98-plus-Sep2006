import Xml2Haskell
import DTD_norback

main = do
  d <- readXml "norback.xml"
  writeXml "-" (d::Test)

  
