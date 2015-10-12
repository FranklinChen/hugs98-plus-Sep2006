module DTD_norback where

import Xml2Haskell


{-Type decls-}

newtype Test = Test [Test_] 		deriving (Eq,Show)
data Test_ = Test_One One
	   | Test_Two Two
	   deriving (Eq,Show)
data One = One 		deriving (Eq,Show)
data Two = Two 		deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Test where
    fromElem (CElem (Elem "test" [] c0):rest) =
	(\(a,ca)->
	   (Just (Test a), rest))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Test a) =
	[CElem (Elem "test" [] (concatMap toElem a))]
instance XmlContent Test_ where
    fromElem c0 =
	case (fromElem c0) of
	(Just a,rest) -> (Just (Test_One a), rest)
	(Nothing,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Test_Two a), rest)
		(Nothing,_) ->
		    (Nothing, c0)
    fromElem rest = (Nothing, rest)
    toElem (Test_One a) = toElem a
    toElem (Test_Two a) = toElem a
instance XmlContent One where
    fromElem (CElem (Elem "one" [] []):rest) =
	(Just One, rest)
    fromElem rest = (Nothing, rest)
    toElem One =
	[CElem (Elem "one" [] [])]
instance XmlContent Two where
    fromElem (CElem (Elem "two" [] []):rest) =
	(Just Two, rest)
    fromElem rest = (Nothing, rest)
    toElem Two =
	[CElem (Elem "two" [] [])]


{-Done-}
