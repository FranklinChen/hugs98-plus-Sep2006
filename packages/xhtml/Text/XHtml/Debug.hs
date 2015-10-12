-- | This module contains functions for displaying
--   HTML as a pretty tree.
module Text.XHtml.Debug where

import Text.XHtml.Internals
import Text.XHtml.Extras
import Text.XHtml.Table
import Text.XHtml.Strict.Elements
import Text.XHtml.Strict.Attributes
import Text.XHtml.Transitional.Elements
import Text.XHtml.Transitional.Attributes

--
-- * Tree Displaying Combinators
--

-- | The basic idea is you render your structure in the form
-- of this tree, and then use treeHtml to turn it into a Html
-- object with the structure explicit.
data HtmlTree
      = HtmlLeaf Html
      | HtmlNode Html [HtmlTree] Html

treeHtml :: [String] -> HtmlTree -> Html
treeHtml colors h = table ! [
                    border 0,
                    cellpadding 0,
                    cellspacing 2] << treeHtml' colors h
     where
      manycolors = scanr (:) []

      treeHtmls :: [[String]] -> [HtmlTree] -> HtmlTable
      treeHtmls c ts = aboves (zipWith treeHtml' c ts)

      treeHtml' :: [String] -> HtmlTree -> HtmlTable
      treeHtml' (c:_) (HtmlLeaf leaf) = cell
                                         (td ! [width "100%"] 
                                            << bold  
                                               << leaf)
      treeHtml' (c:cs@(c2:_)) (HtmlNode hopen ts hclose) =
          if null ts && isNoHtml hclose
          then
              cell hd 
          else if null ts
          then
              hd </> bar `beside` (td ! [bgcolor c2] << spaceHtml)
                 </> tl
          else
              hd </> (bar `beside` treeHtmls morecolors ts)
                 </> tl
        where
              -- This stops a column of colors being the same
              -- color as the immeduately outside nesting bar.
              morecolors = filter ((/= c).head) (manycolors cs)
              bar = td ! [bgcolor c,width "10"] << spaceHtml
              hd = td ! [bgcolor c] << hopen
              tl = td ! [bgcolor c] << hclose
      treeHtml' _ _ = error "The imposible happens"

instance HTML HtmlTree where
      toHtml x = treeHtml treeColors x

-- type "length treeColors" to see how many colors are here.
treeColors = ["#88ccff","#ffffaa","#ffaaff","#ccffff"] ++ treeColors


-- 
-- * Html Debugging Combinators
--

-- | This uses the above tree rendering function, and displays the
-- Html as a tree structure, allowing debugging of what is
-- actually getting produced.
debugHtml :: (HTML a) => a -> Html
debugHtml obj = table ! [border 0] << 
                  ( th ! [bgcolor "#008888"] 
                     << underline
                       << "Debugging Output"
               </>  td << (toHtml (debug' (toHtml obj)))
              )
  where

      debug' :: Html -> [HtmlTree]
      debug' (Html markups) = map debug markups

      debug :: HtmlElement -> HtmlTree
      debug (HtmlString str) = HtmlLeaf (spaceHtml +++
                                              linesToHtml (lines str))
      debug (HtmlTag {
              markupTag = markupTag,
              markupContent = markupContent,
              markupAttrs  = markupAttrs
              }) =
              case markupContent of
                Html [] -> HtmlNode hd [] noHtml
                Html xs -> HtmlNode hd (map debug xs) tl
        where
              args = if null markupAttrs
                     then ""
                     else "  " ++ unwords (map show markupAttrs) 
              hd = font ! [size "1"] << ("<" ++ markupTag ++ args ++ ">")
              tl = font ! [size "1"] << ("</" ++ markupTag ++ ">")
