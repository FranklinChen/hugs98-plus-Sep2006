module Main where

import IO
import Text.XML.HaXml.XmlContent (fWriteXml)
import DTypes

rjn = Person (Name "Rob Noble") (Email "rjn") [
    Rating (SubjectID 1) (Interest ScoreNone) (Skill ScoreLow),
    Rating (SubjectID 2) (Interest ScoreMedium) (Skill ScoreHigh)]
    (Version 1)

main :: IO ()
main =
    fWriteXml "subjdb.xml" rjn
