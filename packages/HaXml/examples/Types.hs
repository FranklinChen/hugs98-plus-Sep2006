module DTypes where

import Text.XML.HaXml.XmlContent hiding (Name)

-- data types for a simple test program

data Person = Person Name Email [Rating] Version {-! derive : XmlContent !-}

newtype Name = Name String {-! derive : XmlContent !-}
newtype Email = Email String {-! derive : XmlContent !-}
newtype Version = Version Int {-! derive : XmlContent !-}

data Rating = Rating SubjectID Interest Skill {-! derive : XmlContent !-}

newtype SubjectID = SubjectID Int {-! derive : XmlContent !-}
newtype Interest = Interest Score {-! derive : XmlContent !-}
newtype Skill = Skill Score {-! derive : XmlContent !-}

data Score = ScoreNone | ScoreLow | ScoreMedium | ScoreHigh {-! derive : XmlContent !-}

