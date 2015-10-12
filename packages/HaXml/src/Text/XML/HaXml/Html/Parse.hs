-- | This is a parser for HTML documents.  Unlike for XML documents, it
--   must include a certain amount of error-correction to account for
--   HTML features like self-terminating tags, unterminated tags, and
--   incorrect nesting.  The input is tokenised by the
--   XML lexer (a separate lexer is not required for HTML).

-- It uses a slightly extended version of the Hutton/Meijer parser
-- combinators.

module Text.XML.HaXml.Html.Parse
  ( htmlParse
  ) where

import Prelude hiding (either,maybe,sequence)
import qualified Prelude (either)
import Maybe hiding (maybe)
import Char (toLower, isSpace, isDigit, isHexDigit)
import Numeric (readDec,readHex)
import Monad

import Text.XML.HaXml.Types
import Text.XML.HaXml.Lex
import Text.XML.HaXml.Posn
import Text.ParserCombinators.Poly

--  #define DEBUG
 
#if defined(DEBUG)
#  if ( defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 502 ) || \
      ( defined(__NHC__) && __NHC__ > 114 ) || defined(__HUGS__)
import Debug.Trace(trace)
#  elif defined(__GLASGOW_HASKELL__)
import IOExts(trace)
#  elif defined(__NHC__) || defined(__HBC__)
import NonStdTrace
#  endif
debug :: Monad m => String -> m ()
debug s = trace s (return ())
#else
debug :: Monad m => String -> m ()
debug s = return ()
#endif


-- | The first argument is the name of the file, the second is the string
--   contents of the file.  The result is the generic representation of
--   an XML document.  Any errors cause program failure with message to stderr.
htmlParse :: String -> String -> Document Posn
htmlParse name = Prelude.either error id . htmlParse' name

-- | The first argument is the name of the file, the second is the string
--   contents of the file.  The result is the generic representation of
--   an XML document.  Any parsing errors are returned in the @Either@ type.
htmlParse' :: String -> String -> Either String (Document Posn)
htmlParse' name = Prelude.either Left (Right . simplify) . fst
                  . runParser document . xmlLex name

---- Document simplification ----

simplify :: Document i -> Document i
simplify (Document p st (Elem n avs cs) ms) =
    Document p st (Elem n avs (deepfilter simp cs)) ms
  where
    simp (CElem (Elem "null" [] []) _) = False
    simp (CElem (Elem  n     _  []) _) | n `elem` ["font","p","i","b","em"
                                                  ,"tt","big","small"] = False
 -- simp (CString False s _) | all isSpace s = False
    simp _ = True
    deepfilter p =
        filter p . map (\c-> case c of
                          CElem (Elem n avs cs) i
                                  -> CElem (Elem n avs (deepfilter p cs)) i
                          _       -> c)

-- opening any of these, they close again immediately
selfclosingtags = ["img","hr","br","meta","col","link","base"
                  ,"param","area","frame","input"]

--closing this, implicitly closes any of those which are contained in it
closeInnerTags =
  [ ("ul",      ["li"])
  , ("ol",      ["li"])
  , ("dl",      ["dt","dd"])
  , ("tr",      ["th","td"])
  , ("div",     ["p"])
  , ("thead",   ["th","tr","td"])
  , ("tfoot",   ["th","tr","td"])
  , ("tbody",   ["th","tr","td"])
  , ("table",   ["th","tr","td","thead","tfoot","tbody"])
  , ("caption", ["p"])
  , ("th",      ["p"])
  , ("td",      ["p"])
  , ("li",      ["p"])
  , ("dt",      ["p"])
  , ("dd",      ["p"])
  , ("object",  ["p"])
  , ("map",     ["p"])
  , ("body",    ["p"])
  ]

--opening this, implicitly closes that
closes :: Name -> Name -> Bool
"a"  `closes` "a"   =  True
"li" `closes` "li"  =  True
"th" `closes`  t    | t `elem` ["th","td"]      =  True
"td" `closes`  t    | t `elem` ["th","td"]      =  True
"tr" `closes`  t    | t `elem` ["th","td","tr"] =  True
"dt" `closes`  t    | t `elem` ["dt","dd"]      =  True
"dd" `closes`  t    | t `elem` ["dt","dd"]      =  True
"form"  `closes` "form"      = True
"label" `closes` "label"     = True
_       `closes` "option"    = True
"thead" `closes` t  | t `elem` ["colgroup"]          = True
"tfoot" `closes` t  | t `elem` ["thead","colgroup"]  = True
"tbody" `closes` t  | t `elem` ["tbody","tfoot","thead","colgroup"] = True
"colgroup" `closes` "colgroup"  = True
t `closes` "p"
    | t `elem` ["p","h1","h2","h3","h4","h5","h6"
               ,"hr","div","ul","dl","ol","table"]  =  True
_ `closes` _ = False



---- Misc ----

fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a



---- Auxiliary Parsing Functions ----

type HParser a = Parser (Posn,TokenT) a

tok :: TokenT -> HParser TokenT
tok t = do (p,t') <- next
           case t' of TokError s    -> report failBad (show t) p t'
                      _ | t'==t     -> return t
                        | otherwise -> report fail (show t) p t'

name :: HParser Name
--name = do {(p,TokName s) <- next; return s}
name = do (p,tok) <- next
          case tok of 
            TokName s  -> return s 
            TokError s -> report failBad "a name" p tok
            _          -> report fail "a name" p tok

string, freetext :: HParser String
string   = do (p,t) <- next
              case t of TokName s -> return s
                        _         -> report fail "text" p t
freetext = do (p,t) <- next
              case t of TokFreeText s -> return s
                        _             -> report fail "text" p t

maybe :: HParser a -> HParser (Maybe a)
maybe p =
    ( p >>= return . Just) `onFail`
    ( return Nothing)

either :: HParser a -> HParser b -> HParser (Either a b)
either p q =
    ( p >>= return . Left) `onFail`
    ( q >>= return . Right)

word :: String -> HParser ()
word s = do { x <- next
            ; case x of
                (p,TokName n)     | s==n -> return ()
                (p,TokFreeText n) | s==n -> return ()
                (p,t@(TokError _)) -> report failBad (show s) p t
                (p,t) -> report fail (show s) p t
            }

posn :: HParser Posn
posn = do { x@(p,_) <- next
          ; reparse [x]
          ; return p
          } `onFail` return noPos

nmtoken :: HParser NmToken
nmtoken = (string `onFail` freetext)

failP, failBadP :: String -> HParser a
failP msg    = do { p <- posn; fail (msg++"\n    at "++show p) }
failBadP msg = do { p <- posn; failBad (msg++"\n    at "++show p) }

report :: (String->HParser a) -> String -> Posn -> TokenT -> HParser a
report fail exp p t = fail ("Expected "++show exp++" but found "++show t
                           ++"\n  at "++show p)

adjustErrP :: HParser a -> (String->String) -> HParser a
p `adjustErrP` f = p `onFail` do pn <- posn
                                 (p `adjustErr` f) `adjustErr` (++show pn)

---- XML Parsing Functions ----

document :: HParser (Document Posn)
document = do
    p     <- prolog `adjustErr` ("unrecognisable XML prolog\n"++)
    es    <- many1 (element "HTML document")
    ms    <- many misc
    return (Document p emptyST (case map snd es of
                                  [e] -> e
                                  es  -> Elem "html" [] (map mkCElem es))
                               ms)
  where mkCElem e = CElem e noPos

comment :: HParser Comment
comment = do
    bracket (tok TokCommentOpen) (tok TokCommentClose) freetext

processinginstruction :: HParser ProcessingInstruction
processinginstruction = do
    tok TokPIOpen
    commit $ do
      n <- string  `onFail` failP "processing instruction has no target"
      f <- freetext
      (tok TokPIClose `onFail` tok TokAnyClose) `onFail` failP "missing ?> or >"
      return (n, f)

cdsect :: HParser CDSect
cdsect = do
    tok TokSectionOpen
    bracket (tok (TokSection CDATAx)) (tok TokSectionClose) chardata

prolog :: HParser Prolog
prolog = do
    x <- maybe xmldecl
    m1 <- many misc
    dtd <- maybe doctypedecl
    m2 <- many misc
    return (Prolog x m1 dtd m2)

xmldecl :: HParser XMLDecl
xmldecl = do
    tok TokPIOpen
    (word "xml" `onFail` word "XML")
    p <- posn
    s <- freetext
    tok TokPIClose `onFail` failBadP "missing ?> in <?xml ...?>"
    (Prelude.either failP return . fst . runParser aux . xmlReLex p) s
  where
    aux = do
      v <- versioninfo  `onFail` failP "missing XML version info"
      e <- maybe encodingdecl
      s <- maybe sddecl
      return (XMLDecl v e s)

versioninfo :: HParser VersionInfo
versioninfo = do
    (word "version" `onFail` word "VERSION")
    tok TokEqual
    bracket (tok TokQuote) (tok TokQuote) freetext

misc :: HParser Misc
misc = 
    oneOf' [ ("<!--comment-->", comment >>= return . Comment)
           , ("<?PI?>",         processinginstruction >>= return . PI)
           ]


-- Question: for HTML, should we disallow in-line DTDs, allowing only externals?
-- Answer: I think so.

doctypedecl :: HParser DocTypeDecl
doctypedecl = do
    tok TokSpecialOpen
    tok (TokSpecial DOCTYPEx)
    commit $ do
      n <- name
      eid <- maybe externalid
--    es <- maybe (bracket (tok TokSqOpen) (tok TokSqClose)) (many markupdecl)
      tok TokAnyClose  `onFail` failP "missing > in DOCTYPE decl"
--    return (DTD n eid (case es of { Nothing -> []; Just e -> e }))
      return (DTD n eid [])

--markupdecl :: HParser MarkupDecl
--markupdecl =
--    ( elementdecl >>= return . Element) `onFail`
--    ( attlistdecl >>= return . AttList) `onFail`
--    ( entitydecl >>= return . Entity) `onFail`
--    ( notationdecl >>= return . Notation) `onFail`
--    ( misc >>= return . MarkupMisc) `onFail`
--    PEREF(MarkupPE,markupdecl)
--
--extsubset :: HParser ExtSubset
--extsubset = do
--    td <- maybe textdecl
--    ds <- many extsubsetdecl
--    return (ExtSubset td ds)
--
--extsubsetdecl :: HParser ExtSubsetDecl
--extsubsetdecl =
--    ( markupdecl >>= return . ExtMarkupDecl) `onFail`
--    ( conditionalsect >>= return . ExtConditionalSect) `onFail`
--    PEREF(ExtPEReference,extsubsetdecl)

sddecl :: HParser SDDecl
sddecl = do
    (word "standalone" `onFail` word "STANDALONE")
    commit $ do
      tok TokEqual `onFail` failP "missing = in 'standalone' decl"
      bracket (tok TokQuote) (tok TokQuote)
              ( (word "yes" >> return True) `onFail`
                (word "no" >> return False) `onFail`
                failP "'standalone' decl requires 'yes' or 'no' value" )




----
-- VERY IMPORTANT NOTE: The stack returned here contains those tags which
-- have been closed implicitly and need to be reopened again at the
-- earliest opportunity.
type Stack = [(Name,[Attribute])]

element :: Name -> HParser (Stack,Element Posn)
element ctx =
  do
    tok TokAnyOpen
    (ElemTag e avs) <- elemtag
    ( if e `closes` ctx then
         -- insert the missing close-tag, fail forward, and reparse.
         ( do debug ("/")
              unparse ([TokEndOpen, TokName ctx, TokAnyClose,
                        TokAnyOpen, TokName e] ++ reformatAttrs avs)
              return ([], Elem "null" [] []))
      else if e `elem` selfclosingtags then
         -- complete the parse straightaway.
         ( do tok TokEndClose	-- self-closing <tag /> 
              debug (e++"[+]")
              return ([], Elem e avs [])) `onFail`
     --  ( do tok TokAnyClose	-- sequence <tag></tag>	(**not HTML?**)
     --       debug (e++"[+")
     --       n <- bracket (tok TokEndOpen) (tok TokAnyClose) name
     --       debug "]"
     --       if e == (map toLower n :: Name) 
     --         then return ([], Elem e avs [])      
     --         else return (error "no nesting in empty tag")) `onFail`
         ( do tok TokAnyClose	-- <tag> with no close (e.g. <IMG>)
              debug (e++"[+]")
              return ([], Elem e avs []))
      else
        (( do tok TokEndClose
              debug (e++"[]")
              return ([], Elem e avs [])) `onFail`
         ( do tok TokAnyClose `onFail` failP "missing > or /> in element tag"
              debug (e++"[")
           -- zz <- many (content e)
           -- n <- bracket (tok TokEndOpen) (tok TokAnyClose) name
              zz <- manyFinally (content e)
                                (tok TokEndOpen)
              n <- name
              commit (tok TokAnyClose)
              debug "]"
              let (ss,cs) = unzip zz
              let s       = if null ss then [] else last ss
              ( if e == (map toLower n :: Name) then
                  do unparse (reformatTags (closeInner e s))
                     debug "^"
                     return ([], Elem e avs cs)
                else
                  do unparse [TokEndOpen, TokName n, TokAnyClose]
                     debug "-"
                     return (((e,avs):s), Elem e avs cs))
         ) `onFail` failP ("failed to repair non-matching tags in context: "++ctx)))

closeInner :: Name -> [(Name,[Attribute])] -> [(Name,[Attribute])]
closeInner c ts =
    case lookup c closeInnerTags of
      (Just these) -> filter ((`notElem` these).fst) ts
      Nothing      -> ts

unparse ts = do p <- posn
                reparse (zip (repeat p) ts)

reformatAttrs avs = concatMap f0 avs
    where f0 (a, AttValue [Left s]) = [TokName a, TokEqual, TokQuote,
                                       TokFreeText s, TokQuote]
reformatTags ts = concatMap f0 ts
    where f0 (t,avs) = [TokAnyOpen, TokName t]++reformatAttrs avs++[TokAnyClose]

content :: Name -> HParser (Stack,Content Posn)
content ctx = do { p <- posn ; content' p ctx }
  where content' p ctx = oneOf'
          [ ( "element", element ctx >>= \(s,e)-> return (s, CElem e p))
          , ( "chardata", chardata >>= \s-> return ([], CString False s p))
          , ( "reference", reference >>= \r-> return ([], CRef r p))
          , ( "cdsect", cdsect >>= \c-> return ([], CString True c p))
          , ( "misc", misc >>= \m->  return ([], CMisc m p))
          ] `adjustErrP` ("when looking for a content item,\n"++)

----
elemtag :: HParser ElemTag
elemtag = do
    n <- name `adjustErrBad` ("malformed element tag\n"++)
    as <- many attribute
    return (ElemTag (map toLower n) as)

attribute :: HParser Attribute
attribute = do
    n <- name
    v <- (do tok TokEqual
             attvalue) `onFail`
         (return (AttValue [Left "TRUE"]))
    return (map toLower n,v)

--elementdecl :: HParser ElementDecl
--elementdecl = do
--    tok TokSpecialOpen
--    tok (TokSpecial ELEMENTx)
--    n <- name `onFail` failP "missing identifier in ELEMENT decl"
--    c <- contentspec `onFail` failP "missing content spec in ELEMENT decl"
--    tok TokAnyClose `onFail` failP "expected > terminating ELEMENT decl"
--    return (ElementDecl n c)
--
--contentspec :: HParser ContentSpec
--contentspec =
--    ( word "EMPTY" >> return EMPTY) `onFail`
--    ( word "ANY" >> return ANY) `onFail`
--    ( mixed >>= return . Mixed) `onFail`
--    ( cp >>= return . ContentSpec) `onFail`
--    PEREF(ContentPE,contentspec)
--
--choice :: HParser [CP]
--choice = do
--    bracket (tok TokBraOpen) (tok TokBraClose)
--            (cp `sepby1` (tok TokPipe))
--
--sequence :: HParser [CP]
--sequence = do
--    bracket (tok TokBraOpen) (tok TokBraClose)
--            (cp `sepby1` (tok TokComma))
--
--cp :: HParser CP
--cp =
--    ( do n <- name
--         m <- modifier
--         return (TagName n m)) `onFail`
--    ( do ss <- sequence
--         m <- modifier
--         return (Seq ss m)) `onFail`
--    ( do cs <- choice
--         m <- modifier
--         return (Choice cs m)) `onFail`
--    PEREF(CPPE,cp)
--
--modifier :: HParser Modifier
--modifier =
--    ( tok TokStar >> return Star) `onFail`
--    ( tok TokQuery >> return Query) `onFail`
--    ( tok TokPlus >> return Plus) `onFail`
--    ( return None)
--
--mixed :: HParser Mixed
--mixed = do
--    tok TokBraOpen
--    tok TokHash
--    word "PCDATA"
--    cont
--  where
--    cont = ( tok TokBraClose >> return PCDATA) `onFail`
--           ( do cs <- many ( do tok TokPipe
--                                n <- name
--                                return n)
--                tok TokBraClose
--                tok TokStar
--                return (PCDATAplus cs))
--
--attlistdecl :: HParser AttListDecl
--attlistdecl = do
--    tok TokSpecialOpen
--    tok (TokSpecial ATTLISTx)
--    n <- name `onFail` failP "missing identifier in ATTLIST"
--    ds <- many attdef
--    tok TokAnyClose `onFail` failP "missing > terminating ATTLIST"
--    return (AttListDecl n ds)
--
--attdef :: HParser AttDef
--attdef = do
--    n <- name
--    t <- atttype `onFail` failP "missing attribute type in attlist defn"
--    d <- defaultdecl
--    return (AttDef n t d)
--
--atttype :: HParser AttType
--atttype =
--    ( word "CDATA" >> return StringType) `onFail`
--    ( tokenizedtype >>= return . TokenizedType) `onFail`
--    ( enumeratedtype >>= return . EnumeratedType)
--
--tokenizedtype :: HParser TokenizedType
--tokenizedtype =
--    ( word "ID" >> return ID) `onFail`
--    ( word "IDREF" >> return IDREF) `onFail`
--    ( word "IDREFS" >> return IDREFS) `onFail`
--    ( word "ENTITY" >> return ENTITY) `onFail`
--    ( word "ENTITIES" >> return ENTITIES) `onFail`
--    ( word "NMTOKEN" >> return NMTOKEN) `onFail`
--    ( word "NMTOKENS" >> return NMTOKENS)
--
--enumeratedtype :: HParser EnumeratedType
--enumeratedtype =
--    ( notationtype >>= return . NotationType) `onFail`
--    ( enumeration >>= return . Enumeration)
--
--notationtype :: HParser NotationType
--notationtype = do
--    word "NOTATION"
--    bracket (tok TokBraOpen) (tok TokBraClose)
--            (name `sepby1` (tok TokPipe))
--
--enumeration :: HParser Enumeration
--enumeration =
--    bracket (tok TokBraOpen) (tok TokBraClose)
--            (nmtoken `sepby1` (tok TokPipe))
--
--defaultdecl :: HParser DefaultDecl
--defaultdecl =
--    ( tok TokHash >> word "REQUIRED" >> return REQUIRED) `onFail`
--    ( tok TokHash >> word "IMPLIED" >> return IMPLIED) `onFail`
--    ( do f <- maybe (tok TokHash >> word "FIXED" >> return FIXED)
--         a <- attvalue
--         return (DefaultTo a f))
--
--conditionalsect :: HParser ConditionalSect
--conditionalsect =
--    ( do tok TokSectionOpen
--         tok (TokSection INCLUDEx)
--         tok TokSqOpen `onFail` failP "missing [ after INCLUDE"
--         i <- extsubsetdecl `onFail` failP "missing ExtSubsetDecl in INCLUDE"
--         tok TokSectionClose `onFail` failP "missing ] after INCLUDE"
--         return (IncludeSect i)) `onFail`
--    ( do tok TokSectionOpen
--         tok (TokSection IGNOREx)
--         tok TokSqOpen `onFail` failP "missing [ after IGNORE"
--         i <- many ignoresectcontents
--         tok TokSectionClose `onFail` failP "missing ] after IGNORE"
--         return (IgnoreSect i))
--
--ignoresectcontents :: HParser IgnoreSectContents
--ignoresectcontents = do
--    i <- ignore
--    is <- many (do tok TokSectionOpen
--                   ic <- ignoresectcontents
--                   tok TokSectionClose
--                   ig <- ignore
--                   return (ic,ig))
--    return (IgnoreSectContents i is)
--
--ignore :: HParser Ignore
--ignore = freetext >>= return . Ignore

reference :: HParser Reference
reference = do
    bracket (tok TokAmp) (tok TokSemi) (freetext >>= val)
  where
    val ('#':'x':i) | all isHexDigit i
                    = return . RefChar . fst . head . readHex $ i
    val ('#':i)     | all isDigit i
                    = return . RefChar . fst . head . readDec $ i
    val name        = return . RefEntity $ name

{-
reference :: HParser Reference
reference =
    ( charref >>= return . RefChar) `onFail`
    ( entityref >>= return . RefEntity)

entityref :: HParser EntityRef
entityref = do
    n <- bracket (tok TokAmp) (tok TokSemi) name
    return n

charref :: HParser CharRef
charref = do
    bracket (tok TokAmp) (tok TokSemi) (freetext >>= readCharVal)
  where
    readCharVal ('#':'x':i) = return . fst . head . readHex $ i
    readCharVal ('#':i)     = return . fst . head . readDec $ i
    readCharVal _           = mzero
-}

--pereference :: HParser PEReference
--pereference = do
--    bracket (tok TokPercent) (tok TokSemi) nmtoken
--
--entitydecl :: HParser EntityDecl
--entitydecl =
--    ( gedecl >>= return . EntityGEDecl) `onFail`
--    ( pedecl >>= return . EntityPEDecl)
--
--gedecl :: HParser GEDecl
--gedecl = do
--    tok TokSpecialOpen
--    tok (TokSpecial ENTITYx)
--    n <- name
--    e <- entitydef `onFail` failP "missing entity defn in G ENTITY decl"
--    tok TokAnyClose `onFail` failP "expected > terminating G ENTITY decl"
--    return (GEDecl n e)
--
--pedecl :: HParser PEDecl
--pedecl = do
--    tok TokSpecialOpen
--    tok (TokSpecial ENTITYx)
--    tok TokPercent
--    n <- name
--    e <- pedef `onFail` failP "missing entity defn in P ENTITY decl"
--    tok TokAnyClose `onFail` failP "expected > terminating P ENTITY decl"
--    return (PEDecl n e)
--
--entitydef :: HParser EntityDef
--entitydef =
--    ( entityvalue >>= return . DefEntityValue) `onFail`
--    ( do eid <- externalid
--         ndd <- maybe ndatadecl
--         return (DefExternalID eid ndd))
--
--pedef :: HParser PEDef
--pedef =
--    ( entityvalue >>= return . PEDefEntityValue) `onFail`
--    ( externalid >>= return . PEDefExternalID)

externalid :: HParser ExternalID
externalid =
    ( do word "SYSTEM"
         s <- systemliteral
         return (SYSTEM s)) `onFail`
    ( do word "PUBLIC"
         p <- pubidliteral
         s <- (systemliteral `onFail` return (SystemLiteral ""))
         return (PUBLIC p s))

--ndatadecl :: HParser NDataDecl
--ndatadecl = do
--    word "NDATA"
--    n <- name
--    return (NDATA n)

textdecl :: HParser TextDecl
textdecl = do
    tok TokPIOpen
    (word "xml" `onFail` word "XML")
    v <- maybe versioninfo
    e <- encodingdecl
    tok TokPIClose `onFail` failP "expected ?> terminating text decl"
    return (TextDecl v e)

--extparsedent :: HParser ExtParsedEnt
--extparsedent = do
--    t <- maybe textdecl
--    (_,c) <- (content "")
--    return (ExtParsedEnt t c)
--
--extpe :: HParser ExtPE
--extpe = do
--    t <- maybe textdecl
--    e <- extsubsetdecl
--    return (ExtPE t e)

encodingdecl :: HParser EncodingDecl
encodingdecl = do
    (word "encoding" `onFail` word "ENCODING")
    tok TokEqual `onFail` failBadP "expected = in 'encoding' decl"
    f <- bracket (tok TokQuote) (tok TokQuote) freetext
    return (EncodingDecl f)

--notationdecl :: HParser NotationDecl
--notationdecl = do
--    tok TokSpecialOpen
--    word "NOTATION"
--    n <- name
--    e <- either externalid publicid
--    tok TokAnyClose `onFail` failP "expected > terminating NOTATION decl"
--    return (NOTATION n e)

publicid :: HParser PublicID
publicid = do
    word "PUBLICID"
    p <- pubidliteral
    return (PUBLICID p)

entityvalue :: HParser EntityValue
entityvalue = do
    evs <- bracket (tok TokQuote) (tok TokQuote) (many ev)
    return (EntityValue evs)

ev :: HParser EV
ev =
    ( freetext >>= return . EVString) `onFail`
--  PEREF(EVPERef,ev) `onFail`
    ( reference >>= return . EVRef)

attvalue :: HParser AttValue
attvalue =
  ( do avs <- bracket (tok TokQuote) (tok TokQuote)
                      (many (either freetext reference))
       return (AttValue avs) ) `onFail`
  ( do v <- nmtoken
       s <- (tok TokPercent >> return "%") `onFail` return ""
       return (AttValue [Left (v++s)]) ) `onFail`
  ( do s <- oneOf [ tok TokPlus >> return "+"
                  , tok TokHash >> return "#"
                  ]
       v <- nmtoken
       return (AttValue [Left (s++v)]) ) `onFail`
  failP "Badly formatted attribute value"

systemliteral :: HParser SystemLiteral
systemliteral = do
    s <- bracket (tok TokQuote) (tok TokQuote) freetext
    return (SystemLiteral s)		-- note: need to fold &...; escapes

pubidliteral :: HParser PubidLiteral
pubidliteral = do
    s <- bracket (tok TokQuote) (tok TokQuote) freetext
    return (PubidLiteral s)		-- note: need to fold &...; escapes

chardata :: HParser CharData
chardata = freetext -- >>= return . CharData

