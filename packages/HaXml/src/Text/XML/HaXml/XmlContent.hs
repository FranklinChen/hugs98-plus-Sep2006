{-# OPTIONS -fglasgow-exts #-}
-- | The class 'XmlContent' is a kind of replacement for Read and Show:
--   it provides conversions between a generic XML tree representation
--   and your own more specialised typeful Haskell data trees.
--
--   If you are starting with a set of Haskell datatypes, use DrIFT to
--   derive instances of this class for you:
--       http:\/\/repetae.net\/john\/computer\/haskell\/DrIFT
--   If you are starting with an XML DTD, use HaXml's tool DtdToHaskell
--   to generate both the Haskell types and the corresponding instances.
--
--   This unified class interface replaces two previous (somewhat similar)
--   classes: Haskell2Xml and Xml2Haskell.  There was no real reason to have
--   separate classes depending on how you originally defined your datatypes.

--   The methods 'toContents' and 'parseContents' convert a value to and from
--   a generic internal representation of an XML document /without/ a DTD.
--   The functions 'toXml' and 'fromXml' convert a value to and from a generic
--   internal representation of an XML document /including/ a DTD.
--   The functions 'readXml' and 'showXml' convert to and from Strings.
--   The functions 'fReadXml' and 'fWriteXml' do the conversion to and from
--   the given filenames.
--   The functions 'hGetXml' and 'hPutXml' do the conversion to and from
--   the given file handles.
--   (See the type signatures.)
--  

module Text.XML.HaXml.XmlContent
  ( -- * Re-export the relevant set of generic XML document type definitions
    Document(..)
  , Element(..)
  , ElemTag(..)
  , Content(..)
  , Attribute(..)
  , AttValue(..)
  , Prolog(..)
  , Reference(..)
  -- * The enabling classes, that define parsing\/unparsing between Haskell
  --   datatypes and a generic XML representation.
  , XmlContent(..)
  , XmlAttributes(..)
  , XmlAttrType(..)
  -- ** Auxiliaries for writing parsers in the XmlContent class
  , module Text.ParserCombinators.Poly
  , XMLParser
  , content, posnElement, element, interior, inElement, text, attributes
  , posnElementWith, elementWith, inElementWith
  , choice, definite -- ???
  -- ** Auxiliaries for generating in the XmlContent class
  , mkElem, mkElemC, mkAttr
  , toText, toCData
  -- ** Auxiliaries for the attribute-related classes
  , maybeToAttr, defaultToAttr
  , definiteA, defaultA, possibleA, fromAttrToStr, toAttrFrStr
  , Defaultable(..)
  , str2attr, attr2str, attval
  , catMaybes	-- re-exported from Maybe
  -- * Whole-document conversion functions
  , toXml, fromXml
  , readXml, showXml
  , fReadXml, fWriteXml
  , hGetXml,  hPutXml
  -- * Explicit representation of Haskell datatype information
  --   (for conversion to a DTD)
  , module Text.XML.HaXml.TypeMapping
  -- * Types useful for some content models
  , List1(..)
  , ANYContent(..)
  ) where

import IO
import Maybe (catMaybes)
import Char  (chr, ord, isSpace)
import List  (intersperse, isPrefixOf, isSuffixOf, partition)

import Text.PrettyPrint.HughesPJ (render)
import qualified Text.XML.HaXml.Pretty as PP

import Text.XML.HaXml.Types
import Text.XML.HaXml.TypeMapping
import Text.XML.HaXml.Posn     (Posn, posInNewCxt)
import Text.XML.HaXml.Pretty   (document)
import Text.XML.HaXml.Parse    (xmlParse, fst3)
import Text.XML.HaXml.Verbatim (Verbatim(verbatim))

import Text.ParserCombinators.Poly

--  #define DEBUG

#if defined(DEBUG)
import Debug.Trace(trace)
debug :: a -> String -> a
v `debug` s = trace s v
#else
v `debug` s = v
#endif


------------------------------------------------------------------------
-- | Read a single attribute called "value".
attval :: (Read a) => Element i -> a
attval (Elem _ [("value",AttValue [Left s])] []) = read s

-- | Generate a single attribute.
mkAttr :: String -> String -> Attribute
mkAttr n v = (n, AttValue [Left v])

-- | Generate an element with no attributes, named for its HType.
mkElem :: XmlContent a => a -> [Content ()] -> Content ()
mkElem x cs  = CElem (Elem (showHType (toHType x) "") [] cs) ()

-- | Generate an element with no attributes, named directly.
mkElemC :: String -> [Content ()] -> Content ()
mkElemC x cs = CElem (Elem x [] cs) ()

-- | Turn a simple string into XML text. 
toText :: String -> [Content ()]
toText s = [CString False s ()]

-- | Turn a string into an XML CDATA section.
--   (i.e. special characters like '&' are preserved without interpretation.)
toCData :: String -> [Content ()]
toCData s = [CString True s ()]
------------------------------------------------------------------------

	-- probably want to write DTD separately from value, and have
	-- easy ways to combine DTD + value into a document, or write
	-- them to separate files.

-- | Read an XML document from a file and convert it to a fully-typed
--   Haskell value.
fReadXml  :: XmlContent a => FilePath -> IO a
fReadXml fp = do
    f <- ( if fp=="-" then return stdin
           else openFile fp ReadMode )
    x <- hGetContents f 
    let (Document _ _ y _) = xmlParse fp x
        y' = CElem y (posInNewCxt fp Nothing)
    either fail return (fst (runParser parseContents [y']))

-- | Write a fully-typed Haskell value to the given file as an XML
--   document.
fWriteXml :: XmlContent a => FilePath -> a -> IO ()
fWriteXml fp x = do
    f <- ( if fp=="-" then return stdout
           else openFile fp WriteMode )
    hPutXml f False x
    hClose f


-- | Read a fully-typed XML document from a string.
readXml :: XmlContent a => String -> Either String a
readXml s =
    let (Document _ _ y _) = xmlParse "string input" s in
    fst (runParser parseContents
                   [CElem y (posInNewCxt "string input" Nothing)])

-- | Convert a fully-typed XML document to a string (without DTD).
showXml :: XmlContent a => Bool -> a -> String
showXml dtd x =
    case toContents x of
      [CElem _ _] -> (render . document) $ toXml dtd x
      _ -> ""


-- | Convert a fully-typed XML document to a string (with or without DTD).
toXml :: XmlContent a => Bool -> a -> Document ()
toXml dtd value =
    let ht = toHType value in
    Document (Prolog (Just (XMLDecl "1.0" Nothing Nothing))
                     [] (if dtd then Just (toDTD ht) else Nothing) [])
             emptyST
             ( case (ht, toContents value) of
                 (Tuple _, cs)       -> Elem (showHType ht "") [] cs
                 (Defined _ _ _, cs) -> Elem (showHType ht "-XML") [] cs
                 (_, [CElem e ()])   -> e )
             []

-- | Read a Haskell value from an XML document, ignoring the DTD and
--   using the Haskell result type to determine how to parse it.
fromXml :: XmlContent a => Document Posn -> Either String a
fromXml (Document _ _ e@(Elem n _ cs) _)
  | "tuple" `isPrefixOf` n = fst (runParser parseContents cs)
  | "-XML"  `isSuffixOf` n = fst (runParser parseContents cs)
  | otherwise = fst (runParser parseContents
                               [CElem e (posInNewCxt "document" Nothing)])


-- | Read a fully-typed XML document from a file handle.
hGetXml :: XmlContent a => Handle -> IO a
hGetXml h = do
    x <- hGetContents h 
    let (Document _ _ y _) = xmlParse "file handle" x
    either fail return
           (fst (runParser parseContents
                           [CElem y (posInNewCxt "file handle" Nothing)]))

-- | Write a fully-typed XML document to a file handle.
hPutXml :: XmlContent a => Handle -> Bool -> a -> IO ()
hPutXml h dtd x = do
 -- y <-  deCont doc
    (hPutStrLn h . render . document)
          (toXml dtd x)
 --       (Document (Prolog Nothing [] Nothing []) emptyST y [])
--where
 -- deCont [CElem x _] = return x
 -- deCont [] = fail "no XML content generated"
 -- deCont _  = fail "too much XML content generated"

------------------------------------------------------------------------

-- | We need a parsing monad for reading generic XML Content into specific
--   datatypes.  This is a specialisation of the Text.ParserCombinators.Poly
--   ones, where the input token type is fixed as XML Content.
type XMLParser a = Parser (Content Posn) a


------------------------------------------------------------------------
-- Some useful parsing combinators
------------------------------------------------------------------------

-- | The most primitive combinator for XMLParser - get one content item.
content :: String -> XMLParser (Content Posn)
content word = next `adjustErr` (++" when expecting "++word)

-- | Get the next content element, checking that it has one of the required
--   tags, using the given matching function. 
--   (Skips over comments and whitespace, rejects text and refs.
--    Also returns position of element.)
posnElementWith :: (String->String->Bool) -> [String]
                   -> XMLParser (Posn, Element Posn)
posnElementWith match tags = do
    { c <- content (formatted tags)
    ; case c of
          CElem e@(Elem t _ _) pos
              | any (match t) tags -> return (pos, e)
              | otherwise          -> fail ("Found a <"++t++">, but expected "
                                           ++formatted tags++"\nat "++show pos)
          CString b s pos
              | not b && all isSpace s -> posnElementWith match tags
							-- ignore blank space
              | otherwise -> fail ("Found text content, but expected "
                                  ++formatted tags++"\ntext is: "++s
                                  ++"\nat "++show pos)
          CRef r pos -> fail ("Found reference, but expected "
                             ++formatted tags++"\nreference is: "++verbatim r
                             ++"\nat "++show pos)
          CMisc _ _ -> posnElementWith match tags  -- skip comments, PIs, etc.
    }
  where
    formatted [t]  = "a <"++t++">"
    formatted tags = "one of"++ concatMap (\t->" <"++t++">") tags

-- | A specialisation of @posnElementWith (==)@.
posnElement :: [String] -> XMLParser (Posn, Element Posn)
posnElement = posnElementWith (==)

-- | Get the next content element, checking that it has one of the required
--   tags.  (Skips over comments and whitespace, rejects text and refs.)
element :: [String] -> XMLParser (Element Posn)
element tags = fmap snd (posnElement tags)
				`debug` ("Element: "++unwords tags++"\n")

-- | Like element, only permits a more flexible match against the tagname.
elementWith :: (String->String->Bool) -> [String] -> XMLParser (Element Posn)
elementWith match tags = fmap snd (posnElementWith match tags)
				`debug` ("Element: "++unwords tags++"\n")

-- | Run an XMLParser on the contents of the given element (i.e. not on the
--   current monadic content sequence), checking that the contents are
--   exhausted, before returning the calculated value within the current
--   parser context.
interior :: Element Posn -> XMLParser a -> XMLParser a
interior (Elem e _ cs) p =
    case runParser p cs of
        (Left msg, _) -> fail msg
        (Right x, []) -> return x
        (Right x, cs@(c:_))
            | all onlyMisc cs -> return x
            | otherwise       -> fail ("Too many elements inside <"++e++"> at\n"
                                      ++show (info c)++"\n"
                                      ++"Found excess: "++verbatim c)
  where onlyMisc (CMisc _ _) = True
        onlyMisc (CString False s _) | all isSpace s = True
        onlyMisc _ = False

-- | A combination of element + interior.
inElement :: String -> XMLParser a -> XMLParser a
inElement tag p = do { e <- element [tag]; commit (interior e p) }

-- | A combination of elementWith + interior.
inElementWith :: (String->String->Bool) -> String -> XMLParser a -> XMLParser a
inElementWith match tag p = do { e <- elementWith match [tag]
                               ; commit (interior e p) }

-- | Do some parsing of the attributes of the given element
attributes :: XmlAttributes a => Element Posn -> XMLParser a
attributes (Elem _ as _) = return (fromAttrs as)

-- | 'text' is a counterpart to 'element', parsing text content if it
--   exists.  Adjacent text and references are coalesced.
text :: XMLParser String
text = text' []
  where text' acc =
          do { c <- content "plain text"
             ; case c of
                 CString _ s _        -> text' (s:acc)
                 CRef (RefChar s) _   -> text' (("&#"++show s++";") :acc)
                 CRef (RefEntity s) _ -> text' (('&':s++";"):acc)
                 CMisc _ _            -> text' acc
                 CElem _ _         -> do { reparse [c] -- put it back!
                                         ; if null acc then fail "empty string"
                                           else return (concat (reverse acc))
                                         }
             }
          `onFail` ( if null acc then fail "empty string"
                     else return (concat (reverse acc)) )


-- | 'choice f p' means if parseElem succeeds, apply f to the result, otherwise
--   use the continuation parser.
choice :: XmlContent a => (a -> b) -> XMLParser b -> XMLParser b
choice cons (P other) =
    P (\cs-> case runParser parseContents cs of
                 (Left msg, _)  -> other cs
                 (Right x, cs') -> (Right (cons x), cs'))

--choice cons other = fmap cons parseContents `onFail` other

-- | not sure this is needed now.   'definite p' previously ensured that
--   an element was definitely present.  Now I think the monad might take
--   care of that for us.
definite :: XmlContent a => XMLParser a -> String -> String -> XMLParser a
definite p inner tag = P (\cs-> case runParser p cs of
                                   (Left msg, cs') -> (Left (False,msg'), cs')
                                   (Right x, cs')  -> (Right x,   cs'))
  where msg' = "content error: expected "++inner++" inside <"++tag
               ++"> element\n"

------------------------------------------------------------------------

-- | The @XmlContent@ class promises that an XML Content element can be
--   converted to and from a Haskell value.
class HTypeable a => XmlContent a where
    -- | Convert from XML to Haskell
    parseContents :: XMLParser a
    -- | Convert from Haskell to XML
    toContents    :: a -> [Content ()]

    -- | Dummy functions (for most types): used /only/ in the Char instance
    --   for coercing lists of Char into String.
    xToChar       :: a -> Char
    xFromChar     :: Char -> a
    xToChar        = error "HaXml.XmlContent.xToChar used in error"
    xFromChar      = error "HaXml.XmlContent.xFromChar used in error"

-- | The @XmlAttributes@ class promises that a list of XML tag attributes
--   can be converted to and from a Haskell value.
class XmlAttributes a where
    fromAttrs :: [Attribute] -> a
    toAttrs   :: a -> [Attribute]
-- | The @XmlAttrType@ class promises that an attribute taking an XML
--   enumerated type can be converted to and from a Haskell value.
class XmlAttrType a where
    fromAttrToTyp :: String -> Attribute -> Maybe a
    toAttrFrTyp   :: String -> a -> Maybe Attribute


------------------------------------------------------------------------
-- Instances for all the standard basic datatypes.
-- These are for Haskell datatypes being derived to go to XML.
-- DtdToHaskell does not use these instances.
------------------------------------------------------------------------

instance XmlContent Bool where
    toContents b   = [CElem (Elem "bool" [mkAttr "value" (show b)] []) ()]
    parseContents = do { e <- element ["bool"] ; return (attval e) }
    
instance XmlContent Int where
    toContents i   = [CElem (Elem "int" [mkAttr "value" (show i)] []) ()]
    parseContents = do { e <- element ["int"] ; return (attval e) }

instance XmlContent Integer where
    toContents i   = [CElem (Elem "integer" [mkAttr "value" (show i)] []) ()]
    parseContents = do { e <- element ["integer"] ; return (attval e) }
    
instance XmlContent Float where
    toContents i   = [CElem (Elem "float" [mkAttr "value" (show i)] []) ()]
    parseContents = do { e <- element ["float"] ; return (attval e) }

instance XmlContent Double where
    toContents i   = [CElem (Elem "double" [mkAttr "value" (show i)] []) ()]
    parseContents = do { e <- element ["double"] ; return (attval e) }

instance XmlContent Char where
    -- NOT in a string
    toContents c   = [CElem (Elem "char" [mkAttr "value" [c]] []) ()]
    parseContents = do { (Elem _ [("value",(AttValue [Left [c]]))] [])
                             <- element ["char"]
                       ; return c
                       }
    -- Only defined for Char and no other types:
    xToChar   = id
    xFromChar = id

instance XmlContent a => XmlContent [a] where
    toContents xs  = case toHType x of
                       (Prim "Char" _) ->
                            [mkElem "string" [CString True (map xToChar xs) ()]]
                       _ -> [mkElem xs (concatMap toContents xs)]
                   where   (x:_) = xs
    parseContents = P (\x ->
        case x of
            (CString _ s _:cs)
                   -> (Right (map xFromChar s), cs)
            (CElem (Elem "string" [] [CString _ s _]) _:cs)
                   -> (Right (map xFromChar s), cs)
            (CElem (Elem e [] xs) _:cs) | "list" `isPrefixOf` e
                   -> scanElements xs
                   where
                  -- scanElements :: [Content] -> (Either String [a],[Content])
                     scanElements [] = (Right [], cs)
                     scanElements es =
                        case runParser parseContents es of
                            (Left msg, es') -> (Left (False,msg), es')
                            (Right x, es') ->
                                case scanElements es' of
                                    (Left msg, cs) -> (Left msg, cs)
                                    (Right xs, cs) -> (Right (x:xs), cs)
            (CElem (Elem e _ _) pos: cs)
                   -> (Left (False
                            ,"Expected a <list-...>, but found a <"++e++"> at\n"
                            ++ show pos), cs)
            (CRef r pos: cs)
                   -> (Left (False
                            ,"Expected a <list-...>, but found a ref "
                            ++verbatim r++" at\n"++ show pos), cs)
            (_:cs) -> ((\ (P p)-> p) parseContents) cs  -- skip comments etc.
            []     -> (Left (False
                            ,"Ran out of input XML whilst secondary parsing")
                      , [])
        )

instance XmlContent () where
    toContents ()  = [CElem (Elem "unit" [] []) ()]
    parseContents = do { element ["unit"]; return () }

instance (XmlContent a, XmlContent b) => XmlContent (a,b) where
    toContents (a,b) = toContents a ++ toContents b
    parseContents    = do
        { a <- parseContents
        ; b <- parseContents
        ; return (a,b)
        }

instance (XmlContent a, XmlContent b, XmlContent c) => XmlContent (a,b,c) where
    toContents (x,y,z) = toContents x ++ toContents y ++ toContents z
    parseContents = do
        { a <- parseContents
        ; b <- parseContents
        ; c <- parseContents
        ; return (a,b,c)
        }

instance (XmlContent a) => XmlContent (Maybe a) where
    toContents m   = [mkElem m (maybe [] toContents m)]
    parseContents = do
        { e <- elementWith (flip isPrefixOf) ["maybe"]
        ; case e of (Elem _ [] []) -> return Nothing
                    (Elem _ [] _)  -> fmap Just (interior e parseContents)
        }

instance (XmlContent a, XmlContent b) => XmlContent (Either a b) where
    toContents v@(Left aa) =
        [mkElemC (showConstr 0 (toHType v)) (toContents aa)]
    toContents v@(Right ab) =
        [mkElemC (showConstr 1 (toHType v)) (toContents ab)]
    parseContents =
        (inElementWith (flip isPrefixOf) "Left"  $ fmap Left  parseContents)
          `onFail`
        (inElementWith (flip isPrefixOf) "Right" $ fmap Right parseContents)

--    do{ e@(Elem t [] _) <- element ["Left","Right"]
--      ; case t of
--          _ | "Left"  `isPrefixOf` t -> fmap Left  (interior e parseContents)
--            | "Right" `isPrefixOf` t -> fmap Right (interior e parseContents)
--      }


------------------------------------------------------------------------
-- Useful auxiliaries for "fromAttributes" 
------------------------------------------------------------------------

-- | If an attribute is defaultable, then it either takes the default
--   value (which is omitted from the output), or a non-default value
--   (which obviously must be printed).
data Defaultable a  = Default a    | NonDefault a    deriving (Eq,Show)

searchMaybe :: (a -> Maybe b) -> [a] -> Maybe b
searchMaybe f [] = Nothing
searchMaybe f (x:xs) =
    let fx = f x in
    case fx of
      Nothing  -> searchMaybe f xs
      (Just _) -> fx

maybeToAttr :: (String->a->Maybe Attribute) -> String -> Maybe a
               -> Maybe Attribute
maybeToAttr to n Nothing  = Nothing
maybeToAttr to n (Just v) = to n v

defaultToAttr :: (String->a->Maybe Attribute) -> String -> Defaultable a
                 -> Maybe Attribute
defaultToAttr to n (Default v)  = Nothing
defaultToAttr to n (NonDefault v) = to n v

definiteA :: (String->Attribute->Maybe a) -> String -> String
             -> [Attribute] -> a
definiteA from tag at as =
    case searchMaybe (from at) as of
      Nothing  -> error ("missing attribute "++at++" in tag <"++tag++">")
      (Just a) -> a

defaultA :: (String->Attribute->Maybe a) -> a -> String
            -> [Attribute] -> Defaultable a
defaultA from def at as =
    case searchMaybe (from at) as of
      Nothing  -> Default def
      (Just a) -> NonDefault a

possibleA :: (String->Attribute->Maybe a) -> String -> [Attribute] -> Maybe a
possibleA from at as = searchMaybe (from at) as

fromAttrToStr :: String -> Attribute -> Maybe String
fromAttrToStr n (n0,v)
        | n == n0   = Just (attr2str v)
        | otherwise = Nothing

toAttrFrStr   :: String -> String -> Maybe Attribute
toAttrFrStr n v = Just (n, str2attr v)

str2attr :: String -> AttValue
str2attr s =
    let f s =
          let (l,r) = span (\c-> not (elem c "\"&<>'")) s
          in if null r then [Left l]
             else Left l: Right (g (head r)): f (tail r)
        g '"'  = RefEntity "quot"
        g '&'  = RefEntity "amp"
        g '<'  = RefEntity "lt"
        g '>'  = RefEntity "gt"
        g '\'' = RefEntity "apos"
    in AttValue (f s)

attr2str :: AttValue -> String          -- really needs symbol table
attr2str (AttValue xs) =
    let f (Left s) = s
        f (Right (RefChar i))        = [chr i]
        f (Right (RefEntity "quot")) = "\""
        f (Right (RefEntity "amp"))  = "&"
        f (Right (RefEntity "lt"))   = "<"
        f (Right (RefEntity "gt"))   = ">"
        f (Right (RefEntity "apos")) = "'"
        f (Right _)                  = "*"  -- Ooops, ST needed here.
    in concatMap f xs

------------------------------------------------------------------------
--  New content-model types
------------------------------------------------------------------------

{-
data OneOf2 a b
data OneOf3 a b c
data OneOf4 a b c d
    ... etc are now defined (with instances) in module OneOfN.
-}

-- | A type corresponding to XML's ANY contentspec. 
--   It is either a list of unconverted xml 'Content' 
--   or some 'XmlContent'-able value.
--
-- Parsing functions (e.g. 'parseContents') will always produce 'UnConverted'.
-- Note: The Show instance for 'UnConverted' uses 'verbatim'.
data ANYContent = forall a . (XmlContent a, Show a) => ANYContent a
                | UnConverted [Content Posn]

instance Show ANYContent where
    show (UnConverted c) = "UnConverted " ++ (show $ map verbatim c)
    show (ANYContent a)  = "ANYContent " ++ (show a)

instance Eq ANYContent where
    a == b = show a == show b

-- | The List1 type represents lists with at least one element.
--   It is required for DTD content models that use + as a modifier.
data List1 a = NonEmpty [a]  deriving (Eq, Show)


------------------------------------------------------------------------
--  Instances for new content-model types
------------------------------------------------------------------------
instance (HTypeable a) => HTypeable (List1 a) where
    toHType m  = Defined "List1" [hx]
                         [Constr "NonEmpty" [hx] [List hx] {-Nothing-}]
               where (NonEmpty x) = m
                     hx = toHType x
instance (XmlContent a) => XmlContent (List1 a) where
    toContents (NonEmpty xs) = concatMap toContents xs
    parseContents = fmap NonEmpty $ many1 parseContents

instance HTypeable ANYContent where
    toHType _      = Prim "ANYContent" "ANY"
instance XmlContent ANYContent where
    toContents (ANYContent a)  = toContents a
    toContents (UnConverted s) = map (fmap (const ())) s
    parseContents = P (\cs -> (Right (UnConverted cs), []))

------------------------------------------------------------------------
--  
------------------------------------------------------------------------
