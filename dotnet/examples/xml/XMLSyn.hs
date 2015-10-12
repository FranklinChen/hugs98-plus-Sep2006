module XMLSyn where

data XMLDoc 
     -- an XML document consists of a document typing + 
     -- some markup using the elements/attributes declared in the header.
 = XMLDoc (Maybe XMLHeader)
          [Markup]
   deriving ( Show )

data XMLHeader
 = XMLHeader
         (Maybe XMLVersionInfo) -- version, encoding and standalone decls / info.
	 [Markup]               -- initial processing instructions and comments
         -- (Maybe DTD)        -- the DTD
	 [Markup]               -- trailing processing instructions and comments
   deriving ( Show )

data XMLVersionInfo 
 = XMLVersionInfo String (Maybe String) (Maybe String)
   deriving ( Show )

data Markup
 = ProcessingInstr String String
 | Comment         String
 | Element         Element
-- | Reference       RefString
 | PEReference     String
 | CDSection       String
 | CharData        String
    -- for debugging purposes
   deriving ( Show )

data Element
 = Elem String [Attribute] (Maybe [Markup])
   deriving ( Show )

data Attribute 
 = Attribute String String
   deriving ( Show )

data RefString
 = AChar     Char
 | AString   String
 | PERef     String
 | EntityRef String
 | CharRef   String
   deriving ( Show )

