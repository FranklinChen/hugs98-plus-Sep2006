module Main(main) where

import Text.Html

libversion = "3.00"

main = makePages

beige :: String
beige = "#FFF4E1"

-- colors used in the Hugs home pages
orange = "#ffaa88"
bluish = "#eeeeff"
greenish = "#638494"

comma, period :: Html
comma  = stringToHtml ", "
period = stringToHtml ". "

openSource = anchor ! [href "http://www.opensource.org/"] << "open source"
hugs = anchor ! [href "http://haskell.org/hugs/"] << "Hugs"
ghc  = anchor ! [href "http://haskell.org/ghc/"] << "GHC"
soe  = anchor ! [href "http://haskell.org/soe/"] << "School of Expression"
libs = anchor ! [href "http://haskell.org/ghc/docs/latest/set/book-hslibs.html"] 
              << "Hugs-GHC Haskell libraries"
-- ffi  = anchor ! [href "http://www.dcs.gla.ac.uk/fp/software/hdirect/ffi-a4.ps.gz"]
--               << "Foreign Function Interface"
ffi      = "Foreign Function Interface"
alastair = anchor ! [href "http://www.reid-consulting-uk.ltd.uk/alastair/"] << "Alastair Reid"
henrik   = anchor ! [href "http://cs-www.cs.yale.edu/homes/nilsson/"] << "Henrik Nilsson"
antony   = anchor ! [href "http://www.apocalypse.org/pub/u/antony/"] << "Antony Courtney"
ulf      = "Ulf Norell"
url_htmllib = "http://www.cse.ogi.edu/~andy/html/intro.htm"


-- This makes pages look (a little) like the Hugs home pages
frameIt :: (HTML a) => String -> a -> Html
frameIt nm theBody
  =   header << thetitle << ("HGL: " ++ nm)
  +++ body ! [bgcolor white] <<
        ( hdr 
        +++ hr
        +++ hlinks pages
        +++ hr
        +++ theBody
        +++ hr
        +++ hlinks pages
        +++ hr
        +++ table![width "100%"] <<
              ( (td![align "left"] << 
                 font![size "1"]   <<
                 ("Copyright " +++ copyright +++ " 1999-2003 " +++ alastair))
              `beside`
                (td![align "right"] <<
                 anchor ! [href url_htmllib] <<
                 font![size "1", color "008888"] <<
                 "Rendered using Haskell HTML Combinators"
               )
              )
         )
     
section :: String -> Html
section nm = table ! [border 0, cellpadding 0, cellspacing 0, width "100%"] 
    << td ! [bgcolor orange]
    << bold 
    << font ! [size "5"]
    << nm

hdr = table ! [cellpadding 0, cellspacing 0, border 0, bgcolor bluish] <<
        (td ! [align "left", width "0%"]  << 
           (table ! [cellpadding 5, cellspacing 0, border 0, bgcolor greenish] <<
            (td << bold << font![size "15", color white, face "Helvetica"] << "HGL")))
        `beside`
        (td ! [align "left", width "100%"]  << 
           (table ! [cellpadding 5, cellspacing 0, border 0, bgcolor bluish] <<
            (td << bold << font![size "15", face "Helvetica", color greenish] << "Graphics Library")))

vlinks :: [(String,URL)] -> HtmlTable
vlinks xs = aboves $ map vlink $ xs
 where
  vlink (label,url) = (td ! [width "0", noshade] << anchor ! [href url] << label)

hlinks :: [(String,URL)] -> Html
hlinks xs = center $ brackets $ punctuate " | " $ map hlink xs
 where
  hlink (label,url) = anchor ! [href url] << font ! [size "1"] << label

punctuate :: (HTML a) => a -> [Html] -> Html
punctuate p []     = noHtml
punctuate p [x]    = x
punctuate p (x:xs) = x +++ p +++ punctuate p xs

brackets :: (HTML a) => a -> Html
brackets xs = "[" +++ xs +++ "]"

signature = hlinks
  [ ("Alastair Reid",                  "http://www.reid-consulting-uk.ltd.uk/alastair/")
  , ("Reid Consulting (UK) Limited",   "http://www.reid-consulting-uk.ltd.uk/")
  ]

adr_signature = address $ concatHtml
  [ anchor ! [href "http://www.reid-consulting-uk.ltd.uk/alastair/"] << "Alastair Reid"
  , comma
  , stringToHtml "alastair@reid-consulting-uk.ltd.uk"
  , comma
  , anchor ! [href "http://www.reid-consulting-uk.ltd.uk/"] << "Reid Consulting (UK) Limited"
  , period
  ]

makePages = do
  writeFile url_homepage   (renderHtml $ frameIt "Home"           home)
  writeFile url_download   (renderHtml $ frameIt "Downloading"    download)
  writeFile url_faq        (renderHtml $ frameIt "FAQ"            faq)
  writeFile url_bugs       (renderHtml $ frameIt "Known Bugs"     bugs)
  writeFile url_bugreports (renderHtml $ frameIt "Reporting Bugs" bugreports)
  writeFile url_docs       (renderHtml $ frameIt "Documentation"  docs)

url_homepage   = "index.html"
url_download   = "downloading.html"
url_faq        = "faq.html"
url_bugs       = "bugs.html"
url_bugreports = "bug-reports.html"
url_docs       = "documentation.html"

pages = 
  [ ("Home Page",                  url_homepage)
  , ("Downloading",                url_download)
  , ("Frequently Asked Questions", url_faq)
  , ("Known Bugs",                 url_bugs)
  , ("Reporting Bugs",             url_bugreports)
--  , ("Documentation",              url_docs)
  ]

home :: Html
home = 
  center << 
    font![size "+1"] <<
    ( font![color red] << "New! "
    +++ "Updated for use with hierarchical libraries and GHC."
    )
  +++ p << ("The HGL gives the programmer access to the most interesting parts of the Win32 and X11 library without exposing the programmer to the pain and anguish usually associated with using these interfaces.  The library is distributed as " +++ openSource +++ " and is suitable for use in teaching and in applications.")
  +++ p << "The library currently supports:"
  +++ unordList
      [ "Filled and unfilled 2-dimensional objects (text, lines, polygons, ellipses)."
      , "Bitmaps (Win32 version only, for now)."
      , "Control over text alignment, fonts, color."
      , "Simple input events (keyboard, mouse, window resize) to support reactivity."
      , "Timers and double-buffering to support simple animation."
      , "Use of concurrency to avoid the usual inversion of the code associated with event-loop programming."
      , "Multiple windows may be handled at one time."
      ]
  +++ p << "To keep the library simple and portable, the library makes no attempt to support:"
  +++ unordList
      [ "User interface widgets (menus, toolbars, dialog boxes, etc.)"
      , "Palette manipulation and other advanced features."
      , "Many kinds of input event."
      ]
  +++ p << "Enjoy!"

download 
  =   section "Downloading and Installing the X11 version"
  +++ p << 
        (   toHtml "The X11 version is available via HTML as "
--         +++ anchor ! [ href $ "downloads/HSHGL-" ++ libversion ++ ".bin.linux.tar.gz" ]
--             << "a Linux binary"
--         +++ ", "
--         +++ anchor ! [ href $ "downloads/HSHGL-" ++ libversion ++ ".bin.freebsd.tar.gz" ]
--             << "a FreeBSD binary"
--         +++ " and as "
        +++ anchor ! [ href $ "downloads/HSHGL-" ++ libversion ++ ".tar.gz" ]
            << "source code"
        +++ "."
--        +++ " Follow the X11 section of the "
--        +++ anchor ! [ href $ "downloads/Install.txt" ]
--            << "installation instructions"
--        +++ "."
        +++ " It has been successfully used with both Linux and FreeBSD."
        )
  +++ p <<
        (   "To build it from source, you will need "
        +++ anchor ! [ href $ "http://haskell.org/greencard/" ]
            << "GreenCard 3.01"
        +++ " and "
        +++ anchor ! [ href $ "http://haskell.org/ghc/" ]
            << "GHC 6.x"
        +++ " as well."
        )
  +++ section "Downloading and Installing the Win32 version"
  +++ p << 
        (   "The latest release doesn't yet work with Win32.  In the meantime, you can use the "
        +++ anchor ! [ href $ "http://cvs.haskell.org/Hugs/downloads/SOE.msi" ]
            << "previous version (2.0.5)"
        +++ "."
        +++ "To use it, simply download and run the installer."
        )
  +++ section "Release History"
  +++ defList
      [ ("6/6/2003: version 3.00"
        , toHtml $ unordList
          [ "Alpha release in preparation for a new major release."
          , "Added support for GHC 6.0 and hierarchical libraries."
          , "Doesn't currently work with Hugs or Win32 (both coming soon)."
          ]
        )
      , ("14/12/2002: version 2.0.5"
        , toHtml $ unordList
          [ "Updated to work with December 2002 release of Hugs"
          ]
        )
      , ("4/9/2001: version 2.0.4"
        , toHtml $ unordList
          [ "Builds under Hugs (September 2001 release onwards) and GHC 5.0"
          , "Added support for key presses both as ASCII characters and as raw key presses.  The latter is added through a new abstract type 'Key' and operations on it."
          , "Dropped the 'Maybe' from the 3rd argument of openWindowEx since X11 doesn't let you omit the size of a window when you create it."
          , "Minor fixes in documentation."
          ]
        )
      , ("25/6/2000: version 2.0.3"
        , toHtml $ unordList
          [ "Removed need for Hugs sourcecode when installing X11 version."
          , "Changed default X11 colors to match default Win32 colors (white on a black background)."
          , "Updated Win32 version to work with up-to-date Win32 library."
          , "Added code from Paul Hudak's School of Expression to demos/SOE."
          ]
        )
      , ("9/4/2000: version 2.0.2"
        , toHtml(
              p << "Tweaked export list of SOEGraphics (removed Time and regionToGraphic, added getWindowTick) and fixed type signature of getKey in documentation"
          )
        )
      , ("20/1/2000: version 2.0.1"
        , toHtml (p << "Initial Win32 release.")
        )
      , ("16/1/2000: version 2.0.0"
        , toHtml (p << "Initial X11 release.")
        )
      ]


faq = defList
  [ ( "Who wrote the Graphics library?"
    , toHtml alastair
      +++ " with bugfixes from: "
      +++ antony
      +++ " and GHC porting by "
      +++ henrik
      +++ "."
    )
  , ( "What License does the Graphics library use?"
    , toHtml 
        (   "We use "
--         +++ anchor ! [ href $ "downloads/License.txt" ]
--             << 
        +++ "the same BSD-style license as Hugs and GHC"
        +++ ". "
        +++ "[If you haven't seen this kind of license before, you might want to consult the " +++ openSource +++ " site for a less lawyerly explanation.]"
        )
     )
  , ( "What differences are there between versions?"
    , toHtml $ defList 
        [ ("Version 1.0"
          , unordList 
            [ "Win32 only."
            , "Provides the declarative Graphic datatype but does not expose the imperative Draw monad on which it is based."
            , "Overloads names like create and destroy using module qualifiers to disambiguate."
            ]
          )
        , ("Version 1.1"
          , unordList
            [ "Win32 only."
            , "Exposes Draw monad."
            , "Quite different from the interface required by Paul Hudak's book `The Haskell School of Expression - Learning Functional Programming through Multimedia' but includes the SOEGraphics module to bridge the gap."
            , "Terrible Documentation (didn't reflect any of the changes)."
            ]
          )
        , ("Version 2.0"
          , unordList
            [ "Supports X11 and Win32."
            , "Exposes Draw monad."
            , "Does not overload names."
            , "Interface is very close to that required by `The Haskell School of Expression'.  Includes the SOEGraphics module to bridge the remaining gap."
            , "Better documentation."
            ]
          )
        , ("Version 3.0"
          , unordList
            [ "Supports X11 (Win32 coming back soon)."
            , "Uses Hierarchical module namespace."
            ]
          )
        ]
    )
  , ( "What compilers does it work with?"
    , toHtml 
        (   "Hugs releases after November 2002 and GHC 6.0."
        +++ "It requires concurrency and either the X11 or the Win32 packages."
        )
     )
  , ( "Do I need Hugs source code to install the library?"
    , toHtml 
        (   "No."
        )
     )
  ]  

bugs = 
  p << (   "Bugs are categorized according to whether they affect the X11, Win32 or both platforms or whether they are portability `bugs'. "
       +++ "If a bug does not appear on this list, please "
       +++ (anchor ! [href url_bugs] << "report it")
       +++ "."
       )

  +++ section "Bugs in the X11 version"
  +++ unordList
       [ p << "Line styles are not yet implemented." 
       ]
  +++ section "Bugs in the Win32 version"
  +++ p << "No known bugs."
--  +++ unordList
--       [ p << "Bug1" 
--       , p << "Bug2" 
--       ]
  +++ section "Bugs in both versions"
  +++ p << "No known bugs."
--  +++ unordList
--       [ p << "Bug1"
--       , p << "Bug2" 
--       ]
  +++ section "Portability Issues"
  +++ p << "The following functions are provided in X11 but not Win32."
  +++ unordList
       [ p << "runGraphicsEx :: String -> IO () -> IO () allows the programmer to specify the display on which windows are drawn." 
       ]
  +++ p << "Programmers should also watch for the following:"
  +++ unordList
       [ toHtml $ p << "Win32 and X11 differ in their treatment of line styles and widths.  X11 guarantees that the style applies to any line width, Win32 only applies line styles to 0-width lines."
       , toHtml $ p << "Font names are usually not portable between Win32 and X11.  Indeed, they may even vary between different Win32 or X11 machines."
       ]

bugreports = 
  p << (   "Please check that the bug is not on the " 
       +++ (anchor ! [href url_bugs] << "list of known bugs")
       +++ "."
       )
  +++ p << (   "If it is not on the list, please send a bug report to "
           +++ bold (toHtml "alastair at reid-consulting-uk.ltd.uk")
           +++ ".  Bug reports should include enough information to reproduce the bug.  This typically includes:"
           )
  +++ unordList 
      [ p << "What machine are you using?  (uname -a, gcc -v)"
      , p << "What version of Hugs are you using? (banner printed when Hugs starts)"
      , p << "What version of the library are you using? (name of tarfile you installed)"
      , p << "What X server are you using (and what can it do)? (xdpyinfo)"
      , p << "What fonts does your X server provide? (xlsfonts)"
      , p << ("What are you doing?  (a " +++ bold (toHtml "short") +++ " program demonstrating the problem)")
      , p << "What is happening and why is this wrong?"
      ]
  +++ p << "Some of this information can be omitted but the faster I can reproduce the problem, the faster it will get fixed."

docs = 
  p << 
        (   toHtml "Documentation is available in "
        +++ anchor ! [ href $ "downloads/graphics-" ++ libversion ++ ".dvi" ] << "dvi"
        +++ ", "
        +++ anchor ! [ href $ "downloads/graphics-" ++ libversion ++ ".ps" ] << "postscript"
        +++ " and "
        +++ anchor ! [ href $ "downloads/graphics-" ++ libversion ++ ".ps.gz" ] << "compressed postscript"
        +++ " formats. "
        +++ "These files are included in the docs subdirectory of the distribution."
        )


-- Release directory:
-- - *.html
-- - downloads/License.txt
-- - downloads/graphics-$libversion.*.tar.gz
-- - downloads/graphics-$libversion.{dvi,ps.gz}
-- - downloads/graphics-current.*  (symlinks to latest version)
--
-- Tarfile format:
-- - graphics-$libversion
-- - /License.txt
-- - /Install.txt
-- - /Readme.txt  (copied from Homepage, add URLs)
-- - /docs/graphics-$libversion.{dvi,ps.gz}
-- - /lib/x11/*
-- - /lib/win32/*
-- - /demos/*
-- - /test/*
