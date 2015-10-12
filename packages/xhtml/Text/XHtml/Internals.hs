-- #hide

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.XHtml.internals
-- Copyright   :  (c) Andy Gill, and the Oregon Graduate Institute of 
--                Science and Technology, 1999-2001,
--                (c) Bjorn Bringert, 2004-2006
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Bjorn Bringert <bjorn@bringert.net>
-- Stability   :  experimental
-- Portability :  portable
--
-- Internals of the XHTML combinator library.
-----------------------------------------------------------------------------
module Text.XHtml.Internals where

import Data.Char
import Data.Monoid

infixr 2 +++  -- combining Html
infixr 7 <<   -- nesting Html
infixl 8 !    -- adding optional arguments

-- 
-- * Data types
--

-- | A important property of Html is that all strings inside the
-- structure are already in Html friendly format.
data HtmlElement
      = HtmlString String
        -- ^ ..just..plain..normal..text... but using &copy; and &amb;, etc.
      | HtmlTag {                   
              markupTag      :: String,
              markupAttrs    :: [HtmlAttr],
              markupContent  :: Html
              }
        -- ^ tag with internal markup

-- | Attributes with name and value.
data HtmlAttr = HtmlAttr String String


newtype Html = Html { getHtmlElements :: [HtmlElement] }


-- 
-- * Classes
--

instance Show Html where
      showsPrec _ html = showString (renderHtmlFragment html)
      showList htmls   = foldr (.) id (map shows htmls)

instance Show HtmlAttr where
      showsPrec _ (HtmlAttr str val) = 
              showString str .
              showString "=" .
              shows val

instance Monoid Html where
    mempty = noHtml
    mappend = (+++)

-- | HTML is the class of things that can be validly put
-- inside an HTML tag. So this can be one or more 'Html' elements,
-- or a 'String', for example.
class HTML a where
      toHtml     :: a -> Html
      toHtmlFromList :: [a] -> Html

      toHtmlFromList xs = Html (concat [ x | (Html x) <- map toHtml xs])

instance HTML Html where
      toHtml a    = a

instance HTML Char where
      toHtml       a = toHtml [a]
      toHtmlFromList []  = Html []
      toHtmlFromList str = Html [HtmlString (stringToHtmlString str)]

instance (HTML a) => HTML [a] where
      toHtml xs = toHtmlFromList xs

class ADDATTRS a where
      (!) :: a -> [HtmlAttr] -> a

instance (ADDATTRS b) => ADDATTRS (a -> b) where
      fn ! attr = \ arg -> fn arg ! attr

instance ADDATTRS Html where
      (Html htmls) ! attr = Html (map addAttrs htmls)
        where
              addAttrs (html@(HtmlTag { markupAttrs = markupAttrs }) )
                              = html { markupAttrs = markupAttrs ++ attr }
              addAttrs html = html


-- 
-- * Html primitives and basic combinators
--

-- | Put something inside an HTML element.
(<<) :: (HTML a) => 
        (Html -> b) -- ^ Parent
     -> a -- ^ Child
     -> b
fn << arg = fn (toHtml arg)


concatHtml :: (HTML a) => [a] -> Html
concatHtml as = Html (concat (map (getHtmlElements.toHtml) as))

-- | Create a piece of HTML which is the concatenation
--   of two things which can be made into HTML.
(+++) :: (HTML a,HTML b) => a -> b -> Html
a +++ b = Html (getHtmlElements (toHtml a) ++ getHtmlElements (toHtml b))

-- | An empty piece of HTML.
noHtml :: Html
noHtml = Html []

-- | Checks whether the given piece of HTML is empty.
isNoHtml :: Html -> Bool
isNoHtml (Html xs) = null xs

-- | Constructs an element with a custom name.
tag :: String -- ^ Element name
    -> Html -- ^ Element contents
    -> Html
tag str       htmls = Html [
      HtmlTag {
              markupTag = str,
              markupAttrs = [],
              markupContent = htmls }]

-- | Constructs an element with a custom name, and
--   without any children.
itag :: String -> Html
itag str = tag str noHtml

emptyAttr :: String -> HtmlAttr
emptyAttr s = HtmlAttr s s

intAttr :: String -> Int -> HtmlAttr
intAttr s i = HtmlAttr s (show i)

strAttr :: String -> String -> HtmlAttr
strAttr s t = HtmlAttr s (stringToHtmlString t)


{-
foldHtml :: (String -> [HtmlAttr] -> [a] -> a) 
      -> (String -> a)
      -> Html
      -> a
foldHtml f g (HtmlTag str attr fmls) 
      = f str attr (map (foldHtml f g) fmls) 
foldHtml f g (HtmlString  str)           
      = g str

-}

-- | Processing Strings into Html friendly things.
stringToHtmlString :: String -> String
stringToHtmlString = concatMap fixChar
    where
      fixChar '<' = "&lt;"
      fixChar '>' = "&gt;"
      fixChar '&' = "&amp;"
      fixChar '"' = "&quot;"
      fixChar c | ord c < 0xff = [c]
      fixChar c = "&#" ++ show (ord c) ++ ";"


-- | This is not processed for special chars. 
-- use stringToHtml or lineToHtml instead, for user strings, 
-- because they understand special chars, like @'<'@.
primHtml :: String -> Html
primHtml x    = Html [HtmlString x]



-- 
-- * Html Rendering
--

-- | Output the HTML without adding newlines or spaces within the markup.
--   This should be the most time and space efficient way to
--   render HTML, though the ouput is quite unreadable.
showHtmlInternal :: HTML html => 
                    String -- ^ DOCTYPE declaration
                 -> html -> String
showHtmlInternal docType theHtml = 
    docType ++ showHtmlFragment (tag "html" << theHtml)

-- | Outputs indented HTML. Because space matters in
--   HTML, the output is quite messy.
renderHtmlInternal :: HTML html => 
                      String  -- ^ DOCTYPE declaration
                   -> html -> String
renderHtmlInternal docType theHtml =
      docType ++ "\n" ++ renderHtmlFragment (tag "html" << theHtml) ++ "\n"

-- | Outputs indented HTML, with indentation inside elements.
--   This can change the meaning of the HTML document, and 
--   is mostly useful for debugging the HTML output.
--   The implementation is inefficient, and you are normally
--   better off using 'showHtml' or 'renderHtml'.
prettyHtmlInternal :: HTML html => 
                      String -- ^ DOCTYPE declaration
                   -> html -> String
prettyHtmlInternal docType theHtml = 
    docType ++ "\n" ++ prettyHtmlFragment (tag "html" << theHtml)

-- | Render a piece of HTML without adding a DOCTYPE declaration
--   or root element. Does not add any extra whitespace.
showHtmlFragment :: HTML html => html -> String
showHtmlFragment h = 
    (foldr (.) id $ map showHtml' $ getHtmlElements $ toHtml h) ""

-- | Render a piece of indented HTML without adding a DOCTYPE declaration
--   or root element. Only adds whitespace where it does not change
--   the meaning of the document.
renderHtmlFragment :: HTML html => html -> String
renderHtmlFragment h = 
    (foldr (.) id $ map (renderHtml' 0) $ getHtmlElements $ toHtml h) ""

-- | Render a piece of indented HTML without adding a DOCTYPE declaration 
--   or a root element.
--   The indentation is done inside elements.
--   This can change the meaning of the HTML document, and 
--   is mostly useful for debugging the HTML output.
--   The implementation is inefficient, and you are normally
--   better off using 'showHtmlFragment' or 'renderHtmlFragment'.
prettyHtmlFragment :: HTML html => html -> String
prettyHtmlFragment = 
    unlines . concat . map prettyHtml' . getHtmlElements . toHtml

-- | Show a single HTML element, without adding whitespace.
showHtml' :: HtmlElement -> ShowS
showHtml' (HtmlString str) = (++) str
showHtml'(HtmlTag { markupTag = name,
                    markupContent = html,
                    markupAttrs = markupAttrs })
    = if isNoHtml html && elem name validHtmlITags
      then renderTag True name markupAttrs ""
      else (renderTag False name markupAttrs ""
            . foldr (.) id (map showHtml' (getHtmlElements html))
            . renderEndTag name "")

renderHtml' :: Int -> HtmlElement -> ShowS
renderHtml' _ (HtmlString str) = (++) str
renderHtml' n (HtmlTag
              { markupTag = name,
                markupContent = html,
                markupAttrs = markupAttrs })
      = if isNoHtml html && elem name validHtmlITags
        then renderTag True name markupAttrs (nl n)
        else (renderTag False name markupAttrs (nl n)
             . foldr (.) id (map (renderHtml' (n+2)) (getHtmlElements html))
             . renderEndTag name (nl n))
    where
      nl n = "\n" ++ replicate (n `div` 8) '\t' 
             ++ replicate (n `mod` 8) ' '


prettyHtml' :: HtmlElement -> [String]
prettyHtml' (HtmlString str) = [str]
prettyHtml' (HtmlTag
              { markupTag = name,
                markupContent = html,
                markupAttrs = markupAttrs })
      = if isNoHtml html && elem name validHtmlITags
        then 
         [rmNL (renderTag True name markupAttrs "" "")]
        else
         [rmNL (renderTag False name markupAttrs "" "")] ++ 
          shift (concat (map prettyHtml' (getHtmlElements html))) ++
         [rmNL (renderEndTag name "" "")]
  where
      shift = map (\x -> "   " ++ x)
      rmNL = filter (/= '\n')


-- | Show a start tag
renderTag :: Bool       -- ^ 'True' if the empty tag shorthand should be used
	  -> String     -- ^ Tag name
	  -> [HtmlAttr] -- ^ Attributes
	  -> String     -- ^ Whitespace to add after attributes
	  -> ShowS
renderTag empty name attrs nl r
      = "<" ++ name ++ showAttrs attrs ++ nl ++ close ++ r
  where
      close = if empty then " />" else ">"

      showAttrs attrs = concat [' ':showPair attr | attr <- attrs ]

      showPair :: HtmlAttr -> String
      showPair (HtmlAttr tag val)
              = tag ++ "=\"" ++ val  ++ "\""

-- | Show an end tag
renderEndTag :: String -- ^ Tag name
	     -> String -- ^ Whitespace to add after tag name
	     -> ShowS
renderEndTag name nl r = "</" ++ name ++ nl ++ ">" ++ r


-- | The names of all elements which can represented using the empty tag
--   short-hand.
validHtmlITags :: [String]
validHtmlITags = [
		  "area",
		  "base",
		  "basefont",
		  "br",
                  "col",
                  "frame",
		  "hr",
		  "img",
		  "input",
                  "isindex",
                  "link",
		  "meta",
		  "param"
		 ]
