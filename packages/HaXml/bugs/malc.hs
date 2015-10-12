module DTD where

import Xml2Haskell


{-Type decls-}

newtype Persons = Persons [Person] 		deriving (Eq,Show)
data Person = Person_Male_FathersName Person_Attrs
				      Male (Maybe FathersName)
	    | Person_Female_MothersName Person_Attrs Female
					(Maybe MothersName)
	    deriving (Eq,Show)
data Person_Attrs = Person_Attrs
    { personId :: Id
    } deriving (Eq,Show)
newtype FathersName = FathersName String 		deriving (Eq,Show)
newtype MothersName = MothersName String 		deriving (Eq,Show)
data Male = Male
    { maleSrc :: (Maybe String)
    , maleAlt :: Alt
    } deriving (Eq,Show)
data Alt = A  |  B
	 deriving (Eq,Show)
data Female = Female 		deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Persons where
    fromElem (CElem (Elem "persons" [] c0):rest) =
	(\(a,ca)->
	   (Just (Persons a), rest))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Persons a) =
	[CElem (Elem "persons" [] (concatMap toElem a))]
instance XmlContent Person where
    fromElem (CElem (Elem "Person" as c0):rest) =
	case (\(a,ca)->
		(\(b,cb)->
		   (a,b,cb))
		(fromElem ca))
	     (fromElem c0) of
	(Nothing,Nothing,_) -> case (\(a,ca)->
				       (\(b,cb)->
					  (a,b,cb))
				       (fromElem ca))
				    (fromElem c0) of
			       (Nothing,Nothing,_) -> (Nothing, c0)
			       (Just a,b,[]) -> (Just (Person_Female_MothersName (fromAttrs as) a
												b), rest)
	(Just a,b,[]) -> (Just (Person_Male_FathersName (fromAttrs as) a
								       b), rest)
    toElem (Person_Male_FathersName as a
				       b) = [CElem (Elem "Person" (toAttrs as) (toElem a
										++
										maybe [] toElem b) )]
    toElem (Person_Female_MothersName as a
					 b) = [CElem (Elem "Person" (toAttrs as) (toElem a
										  ++
										  maybe [] toElem b) )]
instance XmlAttributes Person_Attrs where
    fromAttrs as =
	Person_Attrs
	  { personId = definiteA fromAttrToTyp "Person" "id" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrTyp "id" (personId v)
	]
instance XmlContent FathersName where
    fromElem (CElem (Elem "FathersName" [] c0):rest) =
	(\(a,ca)->
	   (Just (FathersName a), rest))
	(definite fromText "text" "FathersName" c0)
    fromElem rest = (Nothing, rest)
    toElem (FathersName a) =
	[CElem (Elem "FathersName" [] (toText a))]
instance XmlContent MothersName where
    fromElem (CElem (Elem "MothersName" [] c0):rest) =
	(\(a,ca)->
	   (Just (MothersName a), rest))
	(definite fromText "text" "MothersName" c0)
    fromElem rest = (Nothing, rest)
    toElem (MothersName a) =
	[CElem (Elem "MothersName" [] (toText a))]
instance XmlContent Male where
    fromElem (CElem (Elem "Male" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem v =
	[CElem (Elem "Male" (toAttrs v) [])]
instance XmlAttributes Male where
    fromAttrs as =
	Male
	  { maleSrc = possibleA fromAttrToStr "src" as
	  , maleAlt = definiteA fromAttrToTyp "Male" "alt" as
	  }
    toAttrs v = catMaybes 
	[ maybeA toAttrFrStr "src" (maleSrc v)
	, toAttrFrTyp "alt" (maleAlt v)
	]
instance XmlAttrType Alt where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "A" = Just A
	    translate "B" = Just B
	    translate _ = Nothing
    toAttrFrTyp n A = Just (n, str2attr "A")
    toAttrFrTyp n B = Just (n, str2attr "B")
instance XmlContent Female where
    fromElem (CElem (Elem "Female" [] []):rest) =
	(Just Female, rest)
    fromElem rest = (Nothing, rest)
    toElem Female =
	[CElem (Elem "Female" [] [])]


{-Done-}
