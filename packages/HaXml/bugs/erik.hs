module DTD_erik where

import Xml2Haskell


{-Type decls-}

data Kenmerk = Kenmerk Kenmerk_Attrs [Kenmerk_]
	     deriving (Eq,Show)
data Kenmerk_Attrs = Kenmerk_Attrs
    { kenmerkKmc :: String
    , kenmerkGewicht :: (Defaultable String)
    } deriving (Eq,Show)
data Kenmerk_ = Kenmerk_Str String
	      | Kenmerk_Begin Begin
	      | Kenmerk_Eind Eind
	      deriving (Eq,Show)
newtype Begin = Begin [String] 		deriving (Eq,Show)
newtype Eind = Eind String 		deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Kenmerk where
    fromElem (CElem (Elem "kenmerk" as c0):rest) =
	(\(a,ca)->
	   (Just (Kenmerk (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Kenmerk as a) =
	[CElem (Elem "kenmerk" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Kenmerk_Attrs where
    fromAttrs as =
	Kenmerk_Attrs
	  { kenmerkKmc = definiteA fromAttrToStr "kenmerk" "kmc" as
	  , kenmerkGewicht = defaultA fromAttrToStr "1" "gewicht" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "kmc" (kenmerkKmc v)
	, defaultToAttr toAttrFrStr "gewicht" (kenmerkGewicht v)
	]
instance XmlContent Kenmerk_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Kenmerk_Str a), rest)
	(Nothing,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Kenmerk_Begin a), rest)
		(Nothing,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Kenmerk_Eind a), rest)
			(Nothing,_) ->
			    (Nothing, c0)
    toElem (Kenmerk_Str a) = [CElem (Elem "kenmerk" [] (toText a) )]
    toElem (Kenmerk_Begin a) = [CElem (Elem "kenmerk" [] (toElem a) )]
    toElem (Kenmerk_Eind a) = [CElem (Elem "kenmerk" [] (toElem a) )]
instance XmlContent Begin where
    fromElem (CElem (Elem "begin" [] c0):rest) =
	(\(a,ca)->
	   (Just (Begin a), rest))
	(many fromText c0)
    fromElem rest = (Nothing, rest)
    toElem (Begin a) =
	[CElem (Elem "begin" [] (concatMap toText a))]
instance XmlContent Eind where
    fromElem (CElem (Elem "eind" [] c0):rest) =
	(\(a,ca)->
	   (Just (Eind a), rest))
	(definite fromText "text" "eind" c0)
    fromElem rest = (Nothing, rest)
    toElem (Eind a) =
	[CElem (Elem "eind" [] (toText a))]


{-Done-}
