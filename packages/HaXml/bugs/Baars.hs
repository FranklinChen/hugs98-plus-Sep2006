module Main where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Text.XML.HaXml.Wrappers (fix2Args)

main = do
    (infile,outfile) <- fix2Args
    putStrLn ("reading "++infile)
    value <- readXml infile
    putStrLn (let (Descriptions xs) = value in
              if xs == xs then "ok" else "failed")
    print value

{-Type decls-}

newtype Descriptions = Descriptions (Maybe Descriptions_) 		deriving (Eq,Show)
data Descriptions_ = Descriptions_ Item Description
		   deriving (Eq,Show)
newtype Item = Item String 		deriving (Eq,Show)
newtype Description = Description String 		deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Descriptions where
    fromElem (c0@(CElem (Elem "descriptions" [] _)):rest) =
	(\(a,ca)->
	   (Just (Descriptions a), rest))
	(fromElem [c0])
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Descriptions a) =
	[CElem (Elem "descriptions" [] (maybe [] toElem a))]
instance XmlContent Descriptions_ where
    fromElem (CElem (Elem "descriptions" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Descriptions_ a b), rest))
	   (definite fromElem "<description>" "descriptions" ca))
	(definite fromElem "<item>" "descriptions" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Descriptions_ a b) =
	[CElem (Elem "descriptions" [] (toElem a ++
					toElem b))]
instance XmlContent Item where
    fromElem (CElem (Elem "item" [] c0):rest) =
	(\(a,ca)->
	   (Just (Item a), rest))
	(definite fromText "text" "item" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Item a) =
	[CElem (Elem "item" [] (toText a))]
instance XmlContent Description where
    fromElem (CElem (Elem "description" [] c0):rest) =
	(\(a,ca)->
	   (Just (Description a), rest))
	(definite fromText "text" "description" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Description a) =
	[CElem (Elem "description" [] (toText a))]


{-Done-}
