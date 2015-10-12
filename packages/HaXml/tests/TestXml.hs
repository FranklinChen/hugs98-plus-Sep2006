--------------------------------------------------------------------------------
--  $Id: TestXml.hs,v 1.35 2004/07/13 17:32:29 graham Exp $
--
--  Copyright (c) 2004, G. KLYNE.  All rights reserved.
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  TestXml
--  Copyright   :  (c) 2004, Graham Klyne
--  License     :  LGPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This module contains test cases for XML parsing and XML handling libraries.
--
--  The test cases make reference to externally stored test data files.
--
--  The module is designed to be retargetable to alternative XML libraries
--  with reasonable effort:  the main body of test cases is isolated from the
--  details of the XML library used.
--
--------------------------------------------------------------------------------

module Main where

import Text.XML.HaXml.Parse
    ( xmlParse'
    )

import Text.XML.HaXml.SubstitutePE
    ( subIntParamEntities
    )

import Text.XML.HaXml.SubstituteGEFilter
    ( subIntGenEntities
    , subExtGenEntities
    )

import Text.XML.HaXml.Validate
    ( validate
    )

import Text.XML.HaXml.Namespace
    ( processNamespaces
    )

import Text.XML.HaXml.XmlBase
    ( processXmlBase
    )

import Text.XML.HaXml.XmlLang
    ( processXmlLang
    )

import Text.XML.HaXml.Lex
    ( xmlLex
    , xmlLexTextDecl
    , xmlLexEntity
    , Posn(..), testPosn
    )

import Text.XML.HaXml.Traverse
    ( docReplaceContent
    , docErrorContent
    , xmlTreeElements
    , xmlListElements
    , xmlListTextContent
    , filterSingle
    , docContent
    )

import Text.XML.HaXml.Pretty
    ( document
    )

import Text.XML.HaXml.QName
    ( makeQN, showQN
    )

import Text.XML.HaXml.Types

import HUnit
    ( Test(TestCase,TestList,TestLabel)
    , Assertable(..)
    , Assertion
    , assertBool, assertEqual, assertString, assertFailure
    , runTestTT, runTestText, putTextToHandle
    )

import IO
    ( Handle, IOMode(WriteMode)
    , openFile, hClose, hPutStr, hPutStrLn
    )

import List
    ( (\\)
    )

------------------------------------------------------------
--  XML handling interfaces
------------------------------------------------------------
--
--  Subsequent tests are based on these interfaces.
--  Re-implement these interfaces to use the XML package
--  under test.
--

doXmlLexOK :: String -> String -> Bool
doXmlLexOK filepath filedata = not $ null (xmlLex filepath filedata)

doXmlPreOK :: String -> String -> Bool
doXmlPreOK filepath filedata = not $ null $
    (subIntParamEntities filepath . xmlLex filepath) filedata

doXmlParseOK :: String -> String -> Bool
doXmlParseOK filepath filedata =
    either (const False) (const True) (xmlParse' filepath filedata)

doXmlParseFormat :: String -> String -> String
doXmlParseFormat filepath filedata =
    either ("Error: "++) (show . document) (xmlParse' filepath filedata)

doXmlParseGESub :: String -> String -> String
doXmlParseGESub filepath filedata =
    either ("Error: "++) (show . document . replaceContent) (xmlParse' filepath filedata)
    where
        replaceContent (Document p s e) = Document p s (docContent (subExtGenEntities s (CElem e)))
        docContent [CElem e] = e
        docContent []        = errElem "produced no output"
        docContent _         = errElem "produced more than one output"
        errElem err          = Elem (makeQN "error") () [] [CErr err]

doXmlParseGESub1 :: String -> String -> String
doXmlParseGESub1 filepath filedata =
    either ("Error: "++) (show . document . replaceContent) (xmlParse' filepath filedata)
    where
        replaceContent (Document p s e) =
            processXmlLang .
            processXmlBase .
            processNamespaces $
            Document p s (docContent (subExtGenEntities s (CElem e)))
        docContent [CElem e] = e
        docContent []        = errElem "produced no output"
        docContent _         = errElem "produced more than one output"
        errElem err          = Elem (makeQN "error") () [] [CErr err]

doXmlValidate :: String -> String -> [String]
doXmlValidate filepath filedata =
    either (return . ("Error: "++))
           (doValidate . replaceContent) (xmlParse' filepath filedata)
    where
        replaceContent (Document p s e) = Document p s (docContent (subExtGenEntities s (CElem e)))
        docContent [CElem e] = e
        docContent []        = errElem "produced no output"
        docContent _         = errElem "produced more than one output"
        errElem err          = Elem (makeQN "error") () [] [CErr err]
        doValidate (Document (Prolog _ _ (Just dtd)) s e) = validate dtd e
        doValidate _ = ["No DTD in document for validation"]

parseGESubDocument :: String -> String -> Document
parseGESubDocument filepath filedata =
    subContent . either docErrorContent id $ (xmlParse' filepath filedata)
    where
        subContent doc@(Document p s e) =
            docReplaceContent (subExtGenEntities s) doc

------------------------------------------------------------
--  Test case helpers
------------------------------------------------------------

testEq :: (Eq a, Show a) => String -> a -> a -> Test
testEq lab a1 a2 =
    TestCase ( assertEqual ("testEq:"++lab) a1 a2 )

assertParseOK :: String -> Bool -> (Either String Document) -> Assertion
assertParseOK lab ok result =
    if ok then assertEqual lab "OK"     (either id              (const "OK") result)
          else assertEqual lab "error"  (either (const "error") (const "OK") result)

assertValid :: String -> Bool -> [String] -> Assertion
assertValid lab ok [] =
    assertEqual lab (if ok then [] else ["error"]) []
assertValid lab ok result =
    assertEqual lab (if ok then [] else result) result

------------------------------------------------------------
--  XML test case functions
------------------------------------------------------------

testXmlLexOK :: String -> Bool -> String -> Test
testXmlLexOK lab ok filepath = TestCase $
    do  { -- putStrLn ("\nTest "++lab)
        ; s <- catch (readFile filepath)  (error ("Failed reading file "++filepath))
        ; assertEqual lab ok (doXmlLexOK filepath s)
        }

testXmlPreOK :: String -> Bool -> String -> Test
testXmlPreOK lab ok filepath = TestCase $
    do  { -- putStrLn ("\nTest "++lab)
        ; s <- catch (readFile filepath)  (error ("Failed reading file "++filepath))
        ; assertEqual lab ok (doXmlPreOK filepath s)
        }

testXmlParseOK :: String -> Bool -> String -> Test
testXmlParseOK lab ok filepath = TestCase $
    do  { -- putStrLn ("\nTest "++lab)
        ; s <- catch (readFile filepath)  (error ("Failed reading file "++filepath))
        ; assertParseOK lab ok (xmlParse' filepath s)
        }

testXmlFormat :: String -> String -> String -> Test
testXmlFormat lab filepathI filepathF = TestCase $
    do  { si <- readFile filepathI
        -- ; writeFile (filepathF++".tmp") (doXmlParseFormat filepathI si)
        ; sf <- readFile filepathF
        ; assertEqual lab sf (doXmlParseFormat filepathI si)
        }

--  Test substitution of General Entities
testXmlGESub :: String -> String -> String -> Test
testXmlGESub lab filepathI filepathF = TestCase $
    do  { si <- readFile filepathI
        -- ; writeFile (filepathF++".tmp") (doXmlParseGESub filepathI si)
        ; sf <- readFile filepathF
        ; assertEqual lab sf (doXmlParseGESub filepathI si)
        }

--  Test substitution of General Entities
testXmlGESub1 :: String -> String -> String -> Test
testXmlGESub1 lab filepathI filepathF = TestCase $
    do  { si <- readFile filepathI
        -- ; writeFile (filepathF++".tmp") (doXmlParseGESub1 filepathI si)
        ; sf <- readFile filepathF
        ; assertEqual lab sf (doXmlParseGESub1 filepathI si)
        }

--  Test validation following substitution of General Entities
testXmlValid :: String -> Bool -> String -> Test
testXmlValid lab ok filepathI = TestCase $
    do  { si <- readFile filepathI
        -- ; writeFile (filepathF++".tmp") (doXmlValidate filepathI si)
        ; assertValid lab ok (doXmlValidate filepathI si)
        }

--  Test namespace handling.  This test works by namespace-processing a file,
--  building a list of QNames correspondingto the elements and attributes
--  within the file, and comparing that list with a supplied value.

testXmlQNames :: String -> String -> [QName] -> Test
testXmlQNames lab filepathI qns = TestCase $
    do  { si <- readFile filepathI
        ; let doc    = parseGESubDocument filepathI si
        ; let docns  = processNamespaces doc
        ; let docqns = concatMap elemQNs (xmlListElements docns)
        ; assertEqual lab qns docqns
        }
    where
        elemQNs (CElem (Elem en _ ats _)) = en:(map attrQN ats)
        elemQNs _                         = []
        attrQN  (an,_)                    = an

egns, egns1, egns2 :: Namespace
egns  = NS "eg" "http://id.example.org/namespace"
egns1 = NS "eg" "http://id.example.org/ns1"
egns2 = NS "eg" "http://id.example.org/ns2"

mknsQN :: Namespace -> String -> QName
mknsQN ns ln = QN ln (Just ns)


--  Test XML base.  This works like the namespace test
--  (to confirm xml:base attributes are removed), but also
--  includes the xml:base QName for each element in the
--  resulting list.

testXmlBase :: String -> String -> [QName] -> Test
testXmlBase lab filepathI qns = TestCase $
    do  { si <- readFile filepathI
        ; let doc    = parseGESubDocument filepathI si
        ; let docns  = processNamespaces doc
        ; let docbas = processXmlBase docns
        ; let docqns = concatMap elemQNs (xmlListElements docbas)
        ; assertEqual lab qns docqns
        }
    where
        elemQNs (CElem (Elem en ei ats _)) = en:baseQN ei:(map attrQN ats)
        elemQNs _                          = []
        baseQN                             = makeQN . eiBase
        attrQN  (an,_)                     = an

egbas = NS "egbas" "http://id.example.org/"

--  Test XML language.  This works like the namespace test
--  (to confirm xml:base attributes are removed), but also
--  includes an xml:lang-basedQName for each element in the
--  resulting list.

testXmlLang :: String -> String -> [QName] -> Test
testXmlLang lab filepathI qns = TestCase $
    do  { si <- readFile filepathI
        ; let doc    = parseGESubDocument filepathI si
        ; let docns  = processNamespaces doc
        ; let docbas = processXmlBase docns
        ; let doclng = processXmlLang docbas
        ; let docqns = concatMap elemQNs (xmlListElements doclng)
        ; assertEqual lab qns docqns
        }
    where
        elemQNs (CElem (Elem en ei ats _)) = en:baseQN ei:langQN ei
                                             :(map attrQN ats)
        elemQNs _                          = []
        baseQN                             = makeQN . eiBase
        langQN                             = makeQN . eiLang
        attrQN  (an,_)                     = an

--  Test attribute values.
--  Read, parse and namespace-process an XML file,
--  then list all attribute values, checking that each attribute
--  is a single string value.
testAttributes :: String -> String -> [String] -> Test
testAttributes lab filepathI qns = TestCase $
    do  { si <- readFile filepathI
        ; let doc    = parseGESubDocument filepathI si
        ; let docns  = processNamespaces doc
        ; let docqns = concatMap elemAttrs (xmlListElements docns)
        ; assertEqual lab qns docqns
        }
    where
        elemAttrs (CElem (Elem en ei ats _)) = (map attrVal ats)
        elemAttrs _                          = []
        attrVal (_,AttValue [Left av])       = av
        attrVal (_,av)                       = "Bad attr: "++show av

--  Test free-text content
--  Read, parse and namespace-process an XML file,
--  then list all free-text values
testFreetext :: String -> String -> [String] -> Test
testFreetext lab filepathI qns = TestCase $
    do  { si <- readFile filepathI
        ; let doc    = parseGESubDocument filepathI si
        ; let docns  = processNamespaces doc
        ; let docqns = map elemText (xmlListTextContent docns)
        ; assertEqual lab qns docqns
        }
    where
        elemText (CString _ txt) = txt
        elemText (CRef (RefEntity ref)) = "&"++ref++";"
        elemText (CRef (RefChar code))  = "&#"++show code++";"

--  Construct test suites from file and list of suffixes
makeTestXmlParseOK :: String -> Bool -> String -> [String] -> Test
makeTestXmlParseOK lab ok fileroot suffixes =
    TestList [ testXmlParseOK (lab++s) ok (fileroot++s++".xml") | s <- suffixes ]

makeTestXmlValidOK :: String -> Bool -> String -> [String] -> Test
makeTestXmlValidOK lab ok fileroot suffixes =
    TestList [ testXmlValid (lab++s) ok (fileroot++s++".xml") | s <- suffixes ]

------------------------------------------------------------
--  Basic XML parsing tests
------------------------------------------------------------

testXmlLex01 = testXmlLexOK "TestXmlLex01" True  "9x9/xmlData01I.xml"
testXmlLex06 = testXmlPreOK "TestXmlLex06" True  "9x9/xmlData06I.xml"
testXmlLex07 = testXmlPreOK "TestXmlLex07" True  "9x9/xmlData07I.xml"
testXmlLex22 = testXmlPreOK "TestXmlLex22" True  "9x9/xmlData22I.xml"

testXmlParse01 = testXmlParseOK "TestXmlParse01" True  "9x9/xmlData01I.xml"
testXmlParse02 = testXmlParseOK "TestXmlParse02" True  "9x9/xmlData02I.xml"
testXmlParse03 = testXmlParseOK "TestXmlParse03" False "9x9/xmlData03I.xml"
testXmlParse04 = testXmlParseOK "TestXmlParse04" False "9x9/xmlData04I.xml"
testXmlParse05 = testXmlParseOK "TestXmlParse05" False "9x9/xmlData05I.xml"
testXmlParse06 = testXmlParseOK "TestXmlParse06" False "9x9/xmlData06I.xml"
testXmlParse07 = testXmlParseOK "TestXmlParse07" True  "9x9/xmlData07I.xml"
testXmlParse08 = testXmlParseOK "TestXmlParse08" True  "9x9/xmlData08I.xml"
testXmlParse09 = testXmlParseOK "TestXmlParse09" False "9x9/xmlData09I.xml"

--  Internal subset tests
testXmlParse20 = testXmlParseOK "TestXmlParse20" True  "9x9/KAoSOntologiesI.owl"
testXmlParse21 = testXmlParseOK "TestXmlParse21" True  "9x9/xmlData21I.xml"
testXmlParse22 = testXmlParseOK "TestXmlParse22" True  "9x9/xmlData22I.xml"
testXmlParse23 = testXmlParseOK "TestXmlParse23" True  "9x9/xmlData23I.xml"
testXmlParse24 = testXmlParseOK "TestXmlParse24" True  "9x9/xmlData24I.xml"
testXmlParse25 = testXmlParseOK "TestXmlParse25" True  "9x9/xmlData25I.xml"
testXmlParse26 = testXmlParseOK "TestXmlParse26" True  "9x9/xmlData26I.xml"
testXmlParse27 = testXmlParseOK "TestXmlParse27" True  "9x9/xmlData27I.xml"
testXmlParse28 = testXmlParseOK "TestXmlParse28" True  "9x9/xmlData28I.xml"
testXmlParse29 = testXmlParseOK "TestXmlParse29" True  "9x9/xmlData29I.xml"

--  External subset tests
testXmlParse31 = testXmlParseOK "TestXmlParse31" True  "9x9/xmlconf_xmltest_097I.xml"
-- This test requires Internet/HTTP access:
test32uri = "http://dev.w3.org/cvsweb/~checkout~/2001/XML-Test-Suite/xmlconf/xmltest/valid/sa/097.ent?rev=1.1&content-type=text/plain"
testXmlParse32 = testXmlParseOK "TestXmlParse32" True  "9x9/xmlData32I.xml"
--
testXmlParse33 = testXmlParseOK "TestXmlParse33" True  "9x9/xmlData33I.xml"

--  Check namespace tests parse OK
testXmlParse41 = testXmlParseOK "TestXmlParse41" True  "9x9/xmlNamespace01.xml"
testXmlParse42 = testXmlParseOK "TestXmlParse42" True  "9x9/xmlNamespace02.xml"
testXmlParse43 = testXmlParseOK "TestXmlParse43" True  "9x9/xmlNamespace03.xml"
testXmlParse44 = testXmlParseOK "TestXmlParse44" True  "9x9/xmlNamespace04.xml"
testXmlParse45 = testXmlParseOK "TestXmlParse45" True  "9x9/xmlNamespace05.xml"
testXmlParse46 = testXmlParseOK "TestXmlParse46" True  "9x9/xmlNamespace06.xml"
testXmlParse47 = testXmlParseOK "TestXmlParse47" True  "9x9/xmlNamespace07.xml"
testXmlParse48 = testXmlParseOK "TestXmlParse48" True  "9x9/xmlNamespace08.xml"
testXmlParse49 = testXmlParseOK "TestXmlParse49" True  "9x9/xmlNamespace09.xml"
testXmlParse50 = testXmlParseOK "TestXmlParse50" True  "9x9/simple.rdf"

testXmlFormat01 = testXmlFormat "TestXmlFormat01" "9x9/xmlData01I.xml"           "9x9/xmlData01F.xml"
testXmlFormat02 = testXmlFormat "TestXmlFormat02" "9x9/xmlData02I.xml"           "9x9/xmlData02F.xml"
testXmlFormat03 = testXmlFormat "TestXmlFormat03" "9x9/xmlData03I.xml"           "9x9/xmlData03F.xml"
testXmlFormat04 = testXmlFormat "TestXmlFormat04" "9x9/xmlData04I.xml"           "9x9/xmlData04F.xml"
testXmlFormat05 = testXmlFormat "TestXmlFormat05" "9x9/xmlData05I.xml"           "9x9/xmlData05F.xml"
testXmlFormat06 = testXmlFormat "TestXmlFormat06" "9x9/xmlData06I.xml"           "9x9/xmlData06F.xml"
testXmlFormat07 = testXmlFormat "TestXmlFormat07" "9x9/xmlData07I.xml"           "9x9/xmlData07F.xml"
testXmlFormat08 = testXmlFormat "TestXmlFormat08" "9x9/xmlData08I.xml"           "9x9/xmlData08F.xml"
testXmlFormat09 = testXmlFormat "TestXmlFormat09" "9x9/xmlData09I.xml"           "9x9/xmlData09F.xml"

--  Internal subset tests
testXmlFormat20 = testXmlFormat "TestXmlFormat20" "9x9/KAoSOntologiesI.owl"      "9x9/KAoSOntologiesF.owl"
testXmlFormat21 = testXmlFormat "TestXmlFormat21" "9x9/xmlData21I.xml"           "9x9/xmlData21F.xml"
testXmlFormat22 = testXmlGESub  "TestXmlFormat22" "9x9/xmlData22I.xml"           "9x9/xmlData22F.xml"
testXmlFormat23 = testXmlGESub  "TestXmlFormat23" "9x9/xmlData23I.xml"           "9x9/xmlData23F.xml"
testXmlFormat24 = testXmlGESub  "TestXmlFormat24" "9x9/xmlData24I.xml"           "9x9/xmlData24F.xml"
testXmlFormat25 = testXmlGESub  "TestXmlFormat25" "9x9/xmlData25I.xml"           "9x9/xmlData25F.xml"
testXmlFormat26 = testXmlGESub  "TestXmlFormat26" "9x9/xmlData26I.xml"           "9x9/xmlData26F.xml"
testXmlFormat27 = testXmlGESub  "TestXmlFormat27" "9x9/xmlData27I.xml"           "9x9/xmlData27F.xml"
testXmlFormat28 = testXmlGESub  "TestXmlFormat28" "9x9/xmlData28I.xml"           "9x9/xmlData28F.xml"
testXmlFormat29 = testXmlGESub  "TestXmlFormat29" "9x9/xmlData29I.xml"           "9x9/xmlData29F.xml"

--  External subset tests
testXmlFormat31 = testXmlFormat "TestXmlFormat31" "9x9/xmlconf_xmltest_097I.xml" "9x9/xmlconf_xmltest_097F.xml"
testXmlFormat32 = testXmlFormat "TestXmlFormat32" "9x9/xmlData32I.xml"           "9x9/xmlData32F.xml"
testXmlFormat33 = testXmlGESub  "TestXmlFormat33" "9x9/xmlData33I.xml"           "9x9/xmlData33F.xml"

--  Namespace tests
testXmlFormat41 = testXmlGESub  "TestXmlFormat41" "9x9/xmlNamespace01.xml"       "9x9/xmlNamespace01F.xml"
testXmlFormat42 = testXmlGESub  "TestXmlFormat42" "9x9/xmlNamespace02.xml"       "9x9/xmlNamespace02F.xml"
testXmlFormat43 = testXmlGESub  "TestXmlFormat43" "9x9/xmlNamespace03.xml"       "9x9/xmlNamespace03F.xml"
testXmlFormat44 = testXmlGESub  "TestXmlFormat44" "9x9/xmlNamespace04.xml"       "9x9/xmlNamespace04F.xml"
testXmlFormat45 = testXmlGESub  "TestXmlFormat45" "9x9/xmlNamespace05.xml"       "9x9/xmlNamespace05F.xml"
testXmlFormat46 = testXmlGESub  "TestXmlFormat46" "9x9/xmlNamespace06.xml"       "9x9/xmlNamespace06F.xml"
testXmlFormat47 = testXmlGESub  "TestXmlFormat47" "9x9/xmlNamespace07.xml"       "9x9/xmlNamespace07F.xml"
testXmlFormat48 = testXmlGESub  "TestXmlFormat48" "9x9/xmlNamespace08.xml"       "9x9/xmlNamespace08F.xml"
testXmlFormat49 = testXmlGESub  "TestXmlFormat49" "9x9/xmlNamespace09.xml"       "9x9/xmlNamespace09F.xml"

testXmlFormat50 = testXmlGESub  "testXmlFormat50" "9x9/simple.rdf"               "9x9/simpleF.rdf"
testXmlFormat51 = testXmlGESub1 "testXmlFormat51" "9x9/XmlBase01.xml"            "9x9/XmlBase01F.xml"
testXmlFormat52 = testXmlGESub1 "testXmlFormat52" "9x9/XmlBase02.xml"            "9x9/XmlBase02F.xml"
testXmlFormat53 = testXmlGESub1 "testXmlFormat53" "9x9/XmlBase03.xml"            "9x9/XmlBase03F.xml"
testXmlFormat54 = testXmlGESub1 "testXmlFormat54" "9x9/XmlBase04.xml"            "9x9/XmlBase04F.xml"

testXmlFormat61 = testXmlGESub1 "testXmlFormat61" "9x9/XmlLang01.xml"            "9x9/XmlLang01F.xml"
testXmlFormat62 = testXmlGESub1 "testXmlFormat62" "9x9/XmlLang02.xml"            "9x9/XmlLang02F.xml"
testXmlFormat63 = testXmlGESub1 "testXmlFormat63" "9x9/XmlLang03.xml"            "9x9/XmlLang03F.xml"
testXmlFormat64 = testXmlGESub1 "testXmlFormat64" "9x9/XmlLang04.xml"            "9x9/XmlLang04F.xml"

--  Validation tests
testXmlValid07 = testXmlValid "testXmlValid07" True  "9x9/xmlData07I.xml"
testXmlValid21 = testXmlValid "testXmlValid21" True  "9x9/xmlData21I.xml"
testXmlValid22 = testXmlValid "testXmlValid22" False "9x9/xmlData22I.xml"
testXmlValid23 = testXmlValid "testXmlValid23" False "9x9/xmlData23I.xml"
testXmlValid24 = testXmlValid "testXmlValid24" False "9x9/xmlData24I.xml"
testXmlValid25 = testXmlValid "testXmlValid25" True  "9x9/xmlData25I.xml"
testXmlValid26 = testXmlValid "testXmlValid26" False "9x9/xmlData26I.xml"
testXmlValid27 = testXmlValid "testXmlValid27" False "9x9/xmlData27I.xml"
testXmlValid28 = testXmlValid "testXmlValid28" True  "9x9/xmlData28I.xml"
testXmlValid29 = testXmlValid "testXmlValid29" True  "9x9/xmlData29I.xml"

--  Namespace tests
testXmlNamespace01 = testXmlQNames "testXmlNamespace01" "9x9/xmlNamespace01.xml"
    [ mknsQN egns "doc" ]
testXmlNamespace02 = testXmlQNames "testXmlNamespace02" "9x9/xmlNamespace02.xml"
    [ makeQN "doc", mknsQN egns "a1" ]
testXmlNamespace03 = testXmlQNames "testXmlNamespace03" "9x9/xmlNamespace03.xml"
    [ makeQN "doc"
    , mknsQN egns "inner", mknsQN egns "a2"
    , mknsQN egns "deeper", mknsQN egns "a3"
    ]
testXmlNamespace04 = testXmlQNames "testXmlNamespace04" "9x9/xmlNamespace04.xml"
    [ mknsQN egns "doc"
    , mknsQN egns "inner", makeQN "a2"
    , mknsQN egns "deeper", makeQN "a3"
    ]
testXmlNamespace05 = testXmlQNames "testXmlNamespace05" "9x9/xmlNamespace05.xml"
    [ mknsQN egns "doc"
    , mknsQN egns "inner", mknsQN egns "a2"
    , mknsQN egns "deeper", mknsQN egns "a3"
    ]
testXmlNamespace06 = testXmlQNames "testXmlNamespace06" "9x9/xmlNamespace06.xml"
    [ mknsQN egns "doc"
    , mknsQN egns "inner", mknsQN egns "a2"
    , mknsQN egns "deeper", mknsQN egns "a3"
    ]
testXmlNamespace07 = testXmlQNames "testXmlNamespace07" "9x9/xmlNamespace07.xml"
    [ mknsQN egns "doc"
    , mknsQN egns "inner", mknsQN egns "a2"
    , mknsQN egns "deeper", mknsQN egns "a3"
    ]
testXmlNamespace08 = testXmlQNames "testXmlNamespace08" "9x9/xmlNamespace08.xml"
    [ mknsQN egns "doc"
    , mknsQN egns1 "inner", mknsQN egns1 "a2"
    , mknsQN egns2 "deeper", mknsQN egns2 "a3"
    ]
testXmlNamespace09 = testXmlQNames "testXmlNamespace09" "9x9/xmlNamespace09.xml"
    [ mknsQN egns "doc"
    , mknsQN egns1 "inner", mknsQN egns1 "a2"
    ]

--  xml:base tests
testXmlBase01 = testXmlBase "testXmlBase01" "9x9/XmlBase01.xml"
    [ makeQN "doc",         makeQN "9x9/XmlBase01.xml"
    , mknsQN egns "inner",  makeQN "9x9/XmlBase01.xml", mknsQN egns "a2"
    , mknsQN egns "deeper", makeQN "9x9/XmlBase01.xml", mknsQN egns "a3"
    ]
testXmlBase02 = testXmlBase "testXmlBase02" "9x9/XmlBase02.xml"
    [ makeQN "doc",         mknsQN  egbas "base1"
    , mknsQN egns "inner",  mknsQN  egbas "base1", mknsQN egns "a2"
    , mknsQN egns "deeper", mknsQN  egbas "base1", mknsQN egns "a3"
    ]
testXmlBase03 = testXmlBase "testXmlBase03" "9x9/XmlBase03.xml"
    [ makeQN "doc",             makeQN "9x9/XmlBase03.xml"
    , mknsQN egns "inner",      mknsQN  egbas "base1", mknsQN egns "a2"
    , mknsQN egns "deeper",     mknsQN  egbas "base2", mknsQN egns "a3"
    , mknsQN egns "evendeeper", mknsQN  egbas "base2"
    ]
testXmlBase04 = testXmlBase "testXmlBase04" "9x9/XmlBase04.xml"
    [ makeQN "doc",                makeQN "9x9/XmlBase04.xml"
    , mknsQN egns "inner",         mknsQN  egbas "base1", mknsQN egns "a2"
    , mknsQN egns "deeper",        mknsQN  egbas "base2", mknsQN egns "a3"
    , mknsQN egns "evendeeper",    mknsQN  egbas "base2"
    , mknsQN egns "anotherdeeper", mknsQN  egbas "base1"
    ]

--  xml:lang tests
testXmlLang01 = testXmlLang "testXmlLang01" "9x9/XmlLang01.xml"
    [ makeQN "doc",         makeQN "9x9/XmlLang01.xml", makeQN ""
    , mknsQN egns "inner",  makeQN "9x9/XmlLang01.xml", makeQN ""
      , mknsQN egns "a2"
    , mknsQN egns "deeper", makeQN "9x9/XmlLang01.xml", makeQN ""
      , mknsQN egns "a3"
    ]
testXmlLang02 = testXmlLang "testXmlLang02" "9x9/XmlLang02.xml"
    [ makeQN "doc",         mknsQN  egbas "base1", makeQN "en"
    , mknsQN egns "inner",  mknsQN  egbas "base1", makeQN "en"
      , mknsQN egns "a2"
    , mknsQN egns "deeper", mknsQN  egbas "base1", makeQN "en"
      , mknsQN egns "a3"
    ]
testXmlLang03 = testXmlLang "testXmlLang03" "9x9/XmlLang03.xml"
    [ makeQN "doc",             makeQN "9x9/XmlLang03.xml", makeQN ""
    , mknsQN egns "inner",      mknsQN  egbas "base1",  makeQN "fr"
      , mknsQN egns "a2"
    , mknsQN egns "deeper",     mknsQN  egbas "base2",  makeQN "de"
      , mknsQN egns "a3"
    , mknsQN egns "evendeeper", mknsQN  egbas "base2",  makeQN "de"
    ]
testXmlLang04 = testXmlLang "testXmlLang04" "9x9/XmlLang04.xml"
    [ makeQN "doc",                makeQN "9x9/XmlLang04.xml", makeQN ""
    , mknsQN egns "inner",         mknsQN egbas "base1",   makeQN "fr"
      , mknsQN egns "a2"
    , mknsQN egns "deeper",        mknsQN egbas "base2",   makeQN "de"
      , mknsQN egns "a3"
    , mknsQN egns "evendeeper",    mknsQN egbas "base2",   makeQN "de"
    , mknsQN egns "anotherdeeper", mknsQN egbas "base1",   makeQN "fr"
    , mknsQN egns "evendeeper",    makeQN "",              makeQN ""
    ]

--  Attribute-value tests
testXmlAttributes01 = testAttributes "testXmlAttributes01" "9x9/SchemaPart.rdf"
    [ "Resource"
    , "en"
    , "fr"
    , "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
    , "en"
    , "fr"
    , "#Class"
    , "subPropertyOf"
    , "en"
    , "fr"
    , "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property"
    , "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property"
    ]

--  Text-content-value tests
testXmlFreetext01 = testFreetext "testXmlFreetext01" "9x9/SchemaPart.rdf"
    [ "Resource"
    , "Ressource"
    , "The most general class"
    , "type"
    , "type"
    , "Indicates membership of a class"
    , "subPropertyOf"
    , "sousPropri\233\&t\233\&De"
    , "Indicates specialization of properties"
    ]


--  Complete test suite
testXmlParseSuite = TestList
    [ testXmlLex01
    , testXmlLex06
    , testXmlLex07
    , testXmlLex22
    , testXmlParse01
    , testXmlParse02
    , testXmlParse03
    , testXmlParse04
    , testXmlParse05
    , testXmlParse06
    , testXmlParse07
    , testXmlParse08
    , testXmlParse09
    , testXmlParse20
    , testXmlParse21
    , testXmlParse22
    , testXmlParse23
    , testXmlParse24
    , testXmlParse25
    , testXmlParse26
    , testXmlParse27
    , testXmlParse28
    , testXmlParse29
    , testXmlParse31
    -- , testXmlParse32 -- requires internet access, see below
    , testXmlParse33
    , testXmlParse41
    , testXmlParse42
    , testXmlParse43
    , testXmlParse44
    , testXmlParse45
    , testXmlParse46
    , testXmlParse47
    , testXmlParse48
    , testXmlParse49
    , testXmlParse50
    , testXmlFormat01
    , testXmlFormat02
    , testXmlFormat03
    , testXmlFormat04
    , testXmlFormat05
    , testXmlFormat06
    , testXmlFormat07
    , testXmlFormat08
    , testXmlFormat09
    , testXmlFormat20
    , testXmlFormat21
    , testXmlFormat22
    , testXmlFormat23
    , testXmlFormat24
    , testXmlFormat25
    , testXmlFormat26
    , testXmlFormat27
    , testXmlFormat28
    , testXmlFormat29
    , testXmlFormat31
    -- , testXmlFormat32 -- requires internet access, see below
    , testXmlFormat33
    , testXmlFormat41
    , testXmlFormat42
    , testXmlFormat43
    , testXmlFormat44
    , testXmlFormat45
    , testXmlFormat46
    , testXmlFormat47
    , testXmlFormat48
    , testXmlFormat49
    , testXmlFormat50
    , testXmlFormat51
    , testXmlFormat52
    , testXmlFormat53
    , testXmlFormat54
    , testXmlFormat61
    , testXmlFormat62
    , testXmlFormat63
    , testXmlFormat64
    , testXmlValid07
    , testXmlValid21
    , testXmlValid22
    , testXmlValid23
    , testXmlValid24
    , testXmlValid25
    , testXmlValid26
    , testXmlValid27
    , testXmlValid28
    , testXmlValid29
    , testXmlNamespace01
    , testXmlNamespace02
    , testXmlNamespace03
    , testXmlNamespace04
    , testXmlNamespace05
    , testXmlNamespace06
    , testXmlNamespace07
    , testXmlNamespace08
    , testXmlNamespace09
    , testXmlBase01
    , testXmlBase02
    , testXmlBase03
    , testXmlBase04
    , testXmlLang01
    , testXmlLang02
    , testXmlLang03
    , testXmlLang04
    , testXmlAttributes01
    , testXmlFreetext01
    ]

testXmlHTTPSuite = TestList
    [ testXmlParse32    -- HTTP access test
    , testXmlFormat32   -- HTTP access test
    ]

--  The following tests are designed to work with files from the
--  W3C XML test suite, which can be obtained from:
--    http://www.w3.org/XML/Test/
--  Retrieve the test suite archive and unpack the directory structure
--  into a directory from which the test program is run (I use the source
--  code directory:  the archive has all its content in subdirectories).
--
--  The tests are generated from the 3-digit number that is used to form
--  the test suite filename in each case, with tests known not to work being
--  removed from the list.  I expect these omissions to be removed as the
--  parser is refined.
--
--  Note:  at this time, the "Valid" XML test suite is used only for testing
--  well-formedness chacks by the parser.  Additional tests may perform validity
--  checking.

jamesClarkParseSASuite =
    makeTestXmlParseOK "JamesClarkParseWFSA" True "xml-conformance/xmltest/valid/sa/"
        ( map (showNDigits 3) [1..119]
        )

jamesClarkNotWfSASuite =
    makeTestXmlParseOK "jamesClarkNotWfSA" False "xml-conformance/xmltest/not-wf/sa/"
        ( map (showNDigits 3) [1..186] \\
          ["006"                                                -- comment containing '--'
          ,"014"                                                -- Literal '<' in attribute value
          ,"025","026","029"                                    -- content containing ']]>'
          ,"038"                                                -- duplicate attr name (is XML WFC)
          ,"061","062","064","065","066","067","068","069"      -- Missing spaces in DTD
          ,"070"                                                -- <!-- ... --->
          ,"071","075","079","080"                              -- Mutually recursive entities
          ,"072","073","076","077","078"                        -- Entity not declared
          ,"074"                                                -- Entity closes containing element
          ,"081","082"                                          -- Attribute ref external entity
          ,"083","084"                                          -- Entity reference unparsed entity
          ,"090"                                                -- Char ref makes ill-formed content
          ,"092"                                                -- Char ref makes ill-formed attribute
          ,"096"                                                -- Missing space in ?XML PI
          ,"101"                                                -- encoding name format
          ,"102"                                                -- version number format
          ,"103"                                                -- Char ref in entity makes ill-formed
          ,"113"                                                -- Unused PE has ill-formed content
          ,"115","116","117","119","120"                        -- Char ref makes ill-formed ent value
          ,"133","134"                                          -- Extra spaces between tokens
          ,"137"                                                -- Missing space
          ,"140","141"                                          -- Char ref makes bad element name
          ,"147"                                                -- Whitespace before <?xml ... ?>
          ,"153"                                                -- Entity gives invalid <?xml ... ?>
          ,"160","161"                                          -- Violates use of PE in internal subset
          ,"162"                                                -- Unused indirect PE has bad content
          ,"165"                                                -- Missing space before %
          ,"180"        -- Entity used before declaration: fix when processing entities
          ,"181","182"  -- Entity value not "content" production: fix when processing entities
          ,"185"        -- External entity in standalone document: fix when processing entities
          ,"186"                                                -- Missing whitespace between attrs
          ]
        )

jamesClarkValidSASuite =
    makeTestXmlValidOK "JamesClarkValidSA" True "xml-conformance/xmltest/valid/sa/"
        ( map (showNDigits 3) [1..119]
        )

showNDigits :: Int -> Int -> String
showNDigits places val = pad places (show val)
    where
        pad places str = replicate (places-length str) '0' ++ str

------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests = TestList
    [ testXmlParseSuite
    -- , testXmlHTTPSuite          -- requires Internet/HTTP access
    , jamesClarkParseSASuite
    , jamesClarkNotWfSASuite
    , jamesClarkValidSASuite
    ]


main = runTestTT allTests

nwf = runTestTT  jamesClarkNotWfSASuite

check = runTestTT testXmlParseSuite

testValid s = makeTestXmlParseOK "JamesClarkValidSA" True "xml-conformance/xmltest/valid/sa/" [s]

runTestFile t = do
    h <- openFile "a.tmp" WriteMode
    runTestText (putTextToHandle h False) t
    hClose h
tf = runTestFile
tt = runTestTT

xmlLexData :: String -> IO String
xmlLexData filepath =
    do  { s <- catch (readFile filepath)  (error ("Failed reading file "++filepath))
        ; let l = xmlLex filepath s
        ; let r = show (length l) ++ "\n**\n" ++ concatMap ((++"\n") . show) l ++ "\n**\n"
        ; putStrLn r
        ; return r
        }

xmlEntData :: String -> IO String
xmlEntData filepath =
    do  { s <- catch (readFile filepath)  (error ("Failed reading file "++filepath))
        ; let l = xmlLexTextDecl filepath Nothing s
        ; let r = show (length l) ++ "\n**\n" ++ concatMap ((++"\n") . show) l ++ "\n**\n"
        ; putStrLn r
        ; return r
        }

xmlPreData :: String -> String -> IO String
xmlPreData p f =
    do  { let filepath = p++f
        ; filedata <- catch (readFile filepath)  (error ("Failed reading file "++filepath))
        ; let l = (subIntParamEntities filepath . xmlLex filepath) filedata
        ; let r = show (length l) ++ "\n**\n" ++ concatMap ((++"\n") . show) l ++ "\n**\n"
        ; putStrLn r
        ; return r
        }

xmlSymData :: String -> String -> IO String
xmlSymData p f =
    do  { let filepath = p++f
        ; filedata <- catch (readFile filepath)  (error ("Failed reading file "++filepath))
        ; let p = (xmlParse' filepath filedata)
        ; let r = case p of
                (Left  err) -> ("Error: "++err)
                (Right (Document pro sym root)) ->
                    show (length sym) ++ "\n**\n" ++ concatMap ((++"\n") . showste) sym ++ "\n**\n"
        ; putStrLn r
        ; return r
        }
    where
        showste (nam,entdef) = nam++": "++showent entdef++"\n"
        showent (DefEntityValue (EntityValue evs)) = concatMap (("\n  "++) . showev) evs
        showent (DefExternalID bas eid _ )         = ("\n  External base="++bas++", eid="++showeid eid)
        showeid (SYSTEM   (SystemLiteral uri))     = uri
        showeid (PUBLIC _ (SystemLiteral uri))     = uri
        showev  (EVString str)                     = str
        showev  (EVRef (RefEntity nam))            = "&"++nam++";"
        showev  (EVRef (RefChar code))             = "&#"++show code++";"

xmlDocData :: String -> String -> IO String
xmlDocData p f =
    do  { let filepath = p++f
        ; filedata <- catch (readFile filepath)  (error ("Failed reading file "++filepath))
        ; let p = (xmlParse' filepath filedata)
        ; let r = case p of
                (Left  err) -> ("Error: "++err)
                (Right doc) -> (show . document) doc
        ; putStrLn r
        ; return r
        }

xmlSubData :: String -> String -> IO String
xmlSubData p f =
    do  { let filepath = p++f
        ; filedata <- catch (readFile filepath)  (error ("Failed reading file "++filepath))
        ; let p = (xmlParse' filepath filedata)
        ; let r = case p of
                (Left  err) -> ("Error: "++err)
                (Right doc) -> (show . document) (subContent doc)
        ; putStrLn r
        ; return r
        }
    where
        subContent doc@(Document _ s _) =
            docReplaceContent (subExtGenEntities s) doc


validPath = "xml-conformance/xmltest/valid/sa/"
notwfPath = "xml-conformance/xmltest/not-wf/sa/"
localPath = ""

entdata = xmlEntData "9x9/xmlconf_xmltest_097.ent"
lexdata = xmlLexData "9x9/xmlData26I.xml"
predata = xmlPreData localPath "9x9/xmlNamespace05.xml"
symdata = xmlSymData localPath "9x9/xmlNamespace05.xml"
docdata = xmlDocData localPath "9x9/xmlNamespace05.xml"
subdata = xmlSubData localPath "9x9/xmlNamespace05.xml"
lexent  = xmlLexEntity testPosn "abc &def; ghi %jkl; mno"

--------------------------------------------------------------------------------
--
--  Copyright (c) 2004, G. KLYNE.  All rights reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
--  You should have received a copy of the GNU Lesser General Public
--  License along with this library; if not, write to:
--    The Free Software Foundation, Inc.,
--    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--  or view the web page at:
--    http://www.gnu.org/copyleft/lesser.html
--
--------------------------------------------------------------------------------
-- $Source: /file/cvsdev/HaskellUtils/HaXml-1.12/test/TestXml.hs,v $
-- $Author: graham $
-- $Revision: 1.35 $
-- $Log: TestXml.hs,v $
-- Revision 1.35  2004/07/13 17:32:29  graham
-- Add xml:lang processing filter test cases.  Fix some old test cases.
--
-- Revision 1.34  2004/07/12 22:20:09  graham
-- New XML parser test cases.
--
-- Revision 1.33  2004/07/06 21:09:28  graham
-- Add specific Show instances for Namespace and QName.
--
-- Revision 1.32  2004/06/28 20:20:21  graham
-- Added new test cases for recursive substutition detection, and
-- document reformatting after
-- namespace processing.
--
-- Revision 1.31  2004/06/28 20:15:54  graham
-- Reorganized general entity substitution logic to separate traversal from
-- substitution, reprocessing and recursive reprocessing logic.
-- Added detection of recursive entity substitution.
--
-- Revision 1.30  2004/06/24 17:48:36  graham
-- Include document filename/URI in parsed document prolog,
-- for subsequent use as a base URI.
--
-- Revision 1.29  2004/06/24 14:06:57  graham
-- Rearranged various lexing functions to be slightly less obscure in their usage.
-- Factored out common code from entity value and attribute value parsing as
-- a new function 'parseString'.
--
-- Revision 1.28  2004/06/22 14:38:53  graham
-- Basic namespace processing is working.
-- Some problems with attribute handling/normalization still to be fixed.
--
-- Revision 1.27  2004/06/18 15:27:46  graham
-- Validation test suote added.  Minor validation bug fixed.  All tests pass.
--
-- Revision 1.26  2004/06/17 17:20:37  graham
-- Substitution of external general entities tested.
--
-- Revision 1.25  2004/06/17 17:08:38  graham
-- Refactored SubstitutePE.hs into SubstitutePE.hs and EntityHelpers.hs,
-- so common functions can be shared between PE and GE substitution code.
--
-- Revision 1.24  2004/06/17 15:11:35  graham
-- Pass test cases for general entity substitution in attribute values.
--
-- Revision 1.23  2004/06/17 11:40:43  graham
-- Internal general entity substitution now passes all test cases.
--
-- Revision 1.22  2004/06/16 18:17:15  graham
-- Parameter entity and lexical phases re-worked to better support
-- general entity substitution.
-- Passes all but two tricky GE substitution
-- regression tests.
--
-- Revision 1.21  2004/06/15 20:01:39  graham
-- First steps of internal general entity substitution filter are working.
-- Some of the parsing has been re-worked to support this.
-- All regression tests still pass.
--
-- Revision 1.20  2004/06/09 10:30:26  graham
-- HTTP access to external entity tested.
--
-- Revision 1.19  2004/06/08 21:21:59  graham
-- Fixed up grammar for 'contentspec'.  Another test case passes.
--
-- Revision 1.18  2004/06/08 20:20:11  graham
-- Relative filename handling for external entitities now works.
-- URI handling and HTTP access is coded, not fully tested.
--
-- Revision 1.17  2004/06/08 11:35:59  graham
-- External parameter entity substitution test passes.
--
-- Revision 1.16  2004/06/08 11:00:06  graham
-- Internal subset PE tests all pass.
-- NOTE changes from previous test cases:
-- PE definitions are stripped out by PE substitution processing,
-- Ill-formed content in unused PEs is not detected.
--
-- Revision 1.15  2004/06/08 10:42:50  graham
-- Parameter entity definition body submitted to full reLex when defined.
--
-- Revision 1.14  2004/06/07 16:42:28  graham
-- Substitution logic now compiles, but not yet built into PE handling code.
-- Two non-well-formed test cases now fail.
-- Not yet decided if they're important enough to fix.
--
-- Revision 1.13  2004/06/03 14:55:37  graham
-- Re-arrange parameter entity handling to distinguish internal subset usage
-- in the syntax, and to leave parameter entities un-substituted in the parse
-- tree.  Test case testXmlFormat21 changes as a result.
--
-- Revision 1.12  2004/06/03 12:52:21  graham
-- First stage of parameter entity re-work:
-- limit recognition of PEs to designated places in syntax.
--
-- Revision 1.11  2004/06/03 10:44:55  graham
-- Modified Unicode module to return a null character when an invalid or
-- out-of-range UTF-8 sequence is encountered.
--
-- Revision 1.10  2004/06/02 19:34:18  graham
-- Various small XML conformance improvements.
--
-- Revision 1.9  2004/06/02 15:14:37  graham
-- Restricted characters allowed in public identifier literal
--
-- Revision 1.8  2004/06/02 13:49:18  graham
-- Fixed Lex.hs to reject illegal XML characters.  This also fixes some
-- run-time failures occurring when documents containing
-- formfeed
-- characters are presented.
--
-- Revision 1.7  2004/06/02 11:00:43  graham
-- Fixed up some comments and code layout.
--
-- Revision 1.6  2004/06/02 08:39:05  graham
-- Re-worked handling of attribute values so that entitry references
-- can be recognized.
--
-- Revision 1.5  2004/05/28 15:28:16  graham
-- Improved conformance with XML, per conformance tests.
-- All but one of the xmltext/valid/sa tests now pass.
-- There are still several xmltext/not-wf/sa tests that are not detected as
-- incorrect XML, notably problems with attribute value handling.
--
-- Revision 1.4  2004/05/28 10:47:48  graham
-- Changed test harness to report error diagnostics on failure (foir debugging).
-- Fixed lexing problem for names beginning with ':' and '_'.
-- Two additional test cases (012,013) passed.
--
-- Revision 1.3  2004/05/25 21:29:48  graham
-- Refactored parser diagnostics handling.
-- Added new type classes to isolate token details.
-- All previous conformance tests still passed.
--
-- Revision 1.2  2004/05/24 12:42:37  graham
-- Create new module ExtEntity to isolate acess to external entity data.
-- Updated parse module to use this.  All tests passed.
--
-- Revision 1.1  2004/05/24 11:54:03  graham
-- Add HaXml 1.12 to local CVS repository, prior to refactoring.
-- Added CVS tags to source files to help track changes.
--
