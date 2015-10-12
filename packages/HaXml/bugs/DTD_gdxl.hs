module DTD_Gdxl where

import Xml2Haskell
{- patches -}

instance (Show a, Show b) => Show (OneOf2 a b) where
    showsPrec i (OneOfTwo a) = showString "OneOfTwo" . showsPrec i a
    showsPrec i (TwoOfTwo b) = showString "TwoOfTwo" . showsPrec i b

instance (Eq a, Eq b) => Eq (OneOf2 a b) where
    (==) (OneOfTwo a) (OneOfTwo b) = a == b
    (==) (TwoOfTwo a) (TwoOfTwo b) = a == b
    (==) _ _ = False

{-Type decls-}

data Gdxl = Gdxl Gdxl_Attrs [Sequencematch]
		 [Graphdelta] (Maybe Xlinkdelta)
	  deriving (Eq,Show)
data Gdxl_Attrs = Gdxl_Attrs
    { gdxlSourcedoc :: (Maybe String)
    , gdxlTargetdoc :: (Maybe String)
    } deriving (Eq,Show)
data Typedelta = Typedelta (Maybe Xlinkdelta)
			   (Maybe Hrefdelta)
	       deriving (Eq,Show)
data Xlinkdelta = Xlinkdelta
    { xlinkdeltaOldxlink :: String
    , xlinkdeltaNewxlink :: String
    } deriving (Eq,Show)
data Hrefdelta = Hrefdelta
    { hrefdeltaHrefmode :: Hrefdelta_Hrefmode
    , hrefdeltaNewlink :: String
    , hrefdeltaOldlink :: String
    } deriving (Eq,Show)
data Hrefdelta_Hrefmode = Hrefdelta_Hrefmode_Absolute
			   |  Hrefdelta_Hrefmode_Local  | 
			  Hrefdelta_Hrefmode_Relative
			deriving (Eq,Show)
data Sequencematch = Sequencematch
    { sequencematchLength :: (Defaultable String)
    , sequencematchOldstart :: String
    , sequencematchNewstart :: String
    } deriving (Eq,Show)
data Graphdelta = Graphdelta (Maybe Typedelta)
			     Attributedelta [Nodedelta] [Edgedelta] [Reldelta]
			     (Maybe Iddelta) (Maybe Roledelta)
			     (OneOf2 Booldelta Skip) (Maybe Booldelta)
			     (Maybe Edgemodedelta)
		deriving (Eq,Show)
data Iddelta = Iddelta
    { iddeltaOldid :: String
    , iddeltaNewid :: String
    } deriving (Eq,Show)
data Roledelta = Roledelta
    { roledeltaOldrole :: String
    , roledeltaNewrole :: String
    } deriving (Eq,Show)
data Booldelta = Booldelta
    { booldeltaOldbool :: Booldelta_Oldbool
    , booldeltaNewbool :: Booldelta_Newbool
    } deriving (Eq,Show)
data Booldelta_Oldbool = Booldelta_Oldbool_True  | 
			 Booldelta_Oldbool_False
		       deriving (Eq,Show)
data Booldelta_Newbool = Booldelta_Newbool_True  | 
			 Booldelta_Newbool_False
		       deriving (Eq,Show)
data Skip = Skip 		deriving (Eq,Show)
data Edgemodedelta = Edgemodedelta
    { edgemodedeltaOldem :: Edgemodedelta_Oldem
    , edgemodedeltaNewem :: Edgemodedelta_Newem
    } deriving (Eq,Show)
data Edgemodedelta_Oldem = Edgemodedelta_Oldem_Directed
			    |  Edgemodedelta_Oldem_Undirected  | 
			   Edgemodedelta_Oldem_Defaultdirected  | 
			   Edgemodedelta_Oldem_Defaultundirected
			 deriving (Eq,Show)
data Edgemodedelta_Newem = Edgemodedelta_Newem_Directed
			    |  Edgemodedelta_Newem_Undirected  | 
			   Edgemodedelta_Newem_Defaultdirected  | 
			   Edgemodedelta_Newem_Defaultundirected
			 deriving (Eq,Show)
data Nodedelta = Nodedelta (Maybe Typedelta)
			   Attributedelta [Sequencematch] [Graphdelta]
			   (Maybe Iddelta)
	       deriving (Eq,Show)
data Edgedelta = Edgedelta (Maybe Typedelta)
			   Attributedelta [Sequencematch] [Graphdelta]
			   (Maybe Iddelta) (OneOf2 Iddelta Skip) (Maybe Iddelta)
			   (OneOf2 Orderingdelta Skip) (Maybe Orderingdelta)
			   (Maybe Booldelta)
	       deriving (Eq,Show)
data Orderingdelta = Orderingdelta
    { orderingdeltaOldorder :: String
    , orderingdeltaNeworder :: String
    } deriving (Eq,Show)
data Reldelta = Reldelta (Maybe Typedelta)
			 Attributedelta [Sequencematch]
			 (OneOf2 [Graphdelta] Skip) [Sequencematch]
			 [Relenddelta] (Maybe Iddelta) (Maybe Booldelta)
	      deriving (Eq,Show)
data Relenddelta = Relenddelta Attributedelta
			       (Maybe Iddelta) (Maybe Roledelta)
			       (Maybe Directiondelta)
			       (OneOf2 Orderingdelta Skip) (Maybe Orderingdelta)
		 deriving (Eq,Show)
data Directiondelta = Directiondelta
    { directiondeltaOlddirection :: (Maybe Directiondelta_Olddirection)
    , directiondeltaNewdirection :: (Maybe Directiondelta_Newdirection)
    } deriving (Eq,Show)
data Directiondelta_Olddirection = Directiondelta_Olddirection_In
				    |  Directiondelta_Olddirection_Out  | 
				   Directiondelta_Olddirection_None
				 deriving (Eq,Show)
data Directiondelta_Newdirection = Directiondelta_Newdirection_In
				    |  Directiondelta_Newdirection_Out  | 
				   Directiondelta_Newdirection_None
				 deriving (Eq,Show)
newtype Attributedelta = Attributedelta [Attributedelta_] 		deriving (Eq,Show)
data Attributedelta_ = Attributedelta_Reduce Reduce
		     | Attributedelta_Change Change
		     | Attributedelta_Extend Extend
		     deriving (Eq,Show)
data Reduce = Reduce Reduce_Attrs [Attrdelta]
	    deriving (Eq,Show)
data Reduce_Attrs = Reduce_Attrs
    { reduceNewname :: (Maybe String)
    } deriving (Eq,Show)
data Extend = Extend Extend_Attrs [Attrdelta]
	    deriving (Eq,Show)
data Extend_Attrs = Extend_Attrs
    { extendOldname :: (Maybe String)
    } deriving (Eq,Show)
data Attrdelta = Attrdelta Attrdelta_Attrs
			   (Maybe Typedelta) (Maybe Attributedelta)
			   (Maybe Iddelta) (Maybe Kinddelta) (Maybe Valuedelta)
	       deriving (Eq,Show)
data Attrdelta_Attrs = Attrdelta_Attrs
    { attrdeltaName :: String
    } deriving (Eq,Show)
data Change = Change (Maybe Namedelta)
		     (Maybe Typedelta) (Maybe Attributedelta)
		     (Maybe Iddelta) (Maybe Kinddelta) (Maybe Valuedelta)
	    deriving (Eq,Show)
data Kinddelta = Kinddelta
    { kinddeltaOldkind :: (Maybe String)
    , kinddeltaNewkind :: (Maybe String)
    } deriving (Eq,Show)
data Valuedelta = ValuedeltaNewvalue Newvalue
		| ValuedeltaLocdelta Locdelta
		| ValuedeltaBooldelta Booldelta
		| ValuedeltaIntdelta Intdelta
		| ValuedeltaFloatdelta Floatdelta
		| ValuedeltaStringdelta Stringdelta
		| ValuedeltaAltdelta Altdelta
		| ValuedeltaGraphdelta Graphdelta
		deriving (Eq,Show)
data Newvalue = Newvalue Value Value
	      deriving (Eq,Show)
data Value = ValueLocdelta Locdelta
	   | ValueBooldelta Booldelta
	   | ValueIntdelta Intdelta
	   | ValueFloatdelta Floatdelta
	   | ValueStringdelta Stringdelta
	   | ValueAltdelta Altdelta
	   | ValueGraphdelta Graphdelta
	   deriving (Eq,Show)
data Locdelta = Locdelta (Maybe Typedelta)
			 (Maybe Hrefdelta)
	      deriving (Eq,Show)
data Intdelta = Intdelta
    { intdeltaOldint :: String
    , intdeltaNewint :: String
    } deriving (Eq,Show)
data Floatdelta = Floatdelta
    { floatdeltaOldfloat :: String
    , floatdeltaNewfloat :: String
    } deriving (Eq,Show)
data Stringdelta = Stringdelta
    { stringdeltaOldstring :: String
    , stringdeltaNewstring :: String
    } deriving (Eq,Show)
data Namedelta = Namedelta
    { namedeltaOldname :: String
    , namedeltaNewname :: String
    } deriving (Eq,Show)
data Altdelta = Altdelta (Maybe Altchange)
			 [Sequencematch] [(OneOf2 Valuedelta Accumdelta)]
	      deriving (Eq,Show)
data Altchange = Altchange
    { altchangeOldalt :: String
    , altchangeNewalt :: String
    } deriving (Eq,Show)
data Accumdelta = Accumdelta Accumdelta_Attrs
			     [Sequencematch] [Valuedelta]
		deriving (Eq,Show)
data Accumdelta_Attrs = Accumdelta_Attrs
    { accumdeltaAccumkind :: (Defaultable Accumdelta_Accumkind)
    } deriving (Eq,Show)
data Accumdelta_Accumkind = Accumdelta_Accumkind_Unch
			     |  Accumdelta_Accumkind_Seq2set  | 
			    Accumdelta_Accumkind_Seq2bag  | 
			    Accumdelta_Accumkind_Set2seq  | 
			    Accumdelta_Accumkind_Set2bag  | 
			    Accumdelta_Accumkind_Bag2seq  | 
			    Accumdelta_Accumkind_Bag2set
			  deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Gdxl where
    fromElem (CElem (Elem "gdxl" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (Just (Gdxl (fromAttrs as) a b c), rest))
	      (fromElem cb))
	   (many fromElem ca))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Gdxl as a b c) =
	[CElem (Elem "gdxl" (toAttrs as) (concatMap toElem a
					  ++ concatMap toElem b ++
					  maybe [] toElem c))]
instance XmlAttributes Gdxl_Attrs where
    fromAttrs as =
	Gdxl_Attrs
	  { gdxlSourcedoc = possibleA fromAttrToStr "sourcedoc" as
	  , gdxlTargetdoc = possibleA fromAttrToStr "targetdoc" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "sourcedoc" (gdxlSourcedoc v)
	, maybeToAttr toAttrFrStr "targetdoc" (gdxlTargetdoc v)
	]
instance XmlContent Typedelta where
    fromElem (CElem (Elem "typedelta" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Typedelta a b), rest))
	   (fromElem ca))
	(fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Typedelta a b) =
	[CElem (Elem "typedelta" [] (maybe [] toElem a ++
				     maybe [] toElem b))]
instance XmlContent Xlinkdelta where
    fromElem (CElem (Elem "xlinkdelta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "xlinkdelta" (toAttrs as) [])]
instance XmlAttributes Xlinkdelta where
    fromAttrs as =
	Xlinkdelta
	  { xlinkdeltaOldxlink = definiteA fromAttrToStr "xlinkdelta" "oldxlink" as
	  , xlinkdeltaNewxlink = definiteA fromAttrToStr "xlinkdelta" "newxlink" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "oldxlink" (xlinkdeltaOldxlink v)
	, toAttrFrStr "newxlink" (xlinkdeltaNewxlink v)
	]
instance XmlContent Hrefdelta where
    fromElem (CElem (Elem "hrefdelta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "hrefdelta" (toAttrs as) [])]
instance XmlAttributes Hrefdelta where
    fromAttrs as =
	Hrefdelta
	  { hrefdeltaHrefmode = definiteA fromAttrToTyp "hrefdelta" "hrefmode" as
	  , hrefdeltaNewlink = definiteA fromAttrToStr "hrefdelta" "newlink" as
	  , hrefdeltaOldlink = definiteA fromAttrToStr "hrefdelta" "oldlink" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrTyp "hrefmode" (hrefdeltaHrefmode v)
	, toAttrFrStr "newlink" (hrefdeltaNewlink v)
	, toAttrFrStr "oldlink" (hrefdeltaOldlink v)
	]
instance XmlAttrType Hrefdelta_Hrefmode where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "absolute" = Just Hrefdelta_Hrefmode_Absolute
	    translate "local" = Just Hrefdelta_Hrefmode_Local
	    translate "relative" = Just Hrefdelta_Hrefmode_Relative
	    translate _ = Nothing
    toAttrFrTyp n Hrefdelta_Hrefmode_Absolute = Just (n, str2attr "absolute")
    toAttrFrTyp n Hrefdelta_Hrefmode_Local = Just (n, str2attr "local")
    toAttrFrTyp n Hrefdelta_Hrefmode_Relative = Just (n, str2attr "relative")
instance XmlContent Sequencematch where
    fromElem (CElem (Elem "sequencematch" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "sequencematch" (toAttrs as) [])]
instance XmlAttributes Sequencematch where
    fromAttrs as =
	Sequencematch
	  { sequencematchLength = defaultA fromAttrToStr "1" "length" as
	  , sequencematchOldstart = definiteA fromAttrToStr "sequencematch" "oldstart" as
	  , sequencematchNewstart = definiteA fromAttrToStr "sequencematch" "newstart" as
	  }
    toAttrs v = catMaybes 
	[ defaultToAttr toAttrFrStr "length" (sequencematchLength v)
	, toAttrFrStr "oldstart" (sequencematchOldstart v)
	, toAttrFrStr "newstart" (sequencematchNewstart v)
	]
instance XmlContent Graphdelta where
    fromElem (CElem (Elem "graphdelta" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (\(d,cd)->
		    (\(e,ce)->
		       (\(f,cf)->
			  (\(g,cg)->
			     (\(h,ch)->
				(\(i,ci)->
				   (\(j,cj)->
				      (Just (Graphdelta a b c d e f g h i
							j), rest))
				   (fromElem ci))
				(fromElem ch))
			     (fromElem cg))
			  (fromElem cf))
		       (fromElem ce))
		    (many fromElem cd))
		 (many fromElem cc))
	      (many fromElem cb))
	   (definite fromElem "<attributedelta>" "graphdelta" ca))
	(fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Graphdelta a b c d e f g h i j) =
	[CElem (Elem "graphdelta" [] (maybe [] toElem a ++
				      toElem b ++ concatMap toElem c ++
				      concatMap toElem d ++ concatMap toElem e
				      ++ maybe [] toElem f ++ maybe [] toElem g
				      ++ toElem h ++ maybe [] toElem i ++
				      maybe [] toElem j))]
instance XmlContent Iddelta where
    fromElem (CElem (Elem "iddelta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "iddelta" (toAttrs as) [])]
instance XmlAttributes Iddelta where
    fromAttrs as =
	Iddelta
	  { iddeltaOldid = definiteA fromAttrToStr "iddelta" "oldid" as
	  , iddeltaNewid = definiteA fromAttrToStr "iddelta" "newid" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "oldid" (iddeltaOldid v)
	, toAttrFrStr "newid" (iddeltaNewid v)
	]
instance XmlContent Roledelta where
    fromElem (CElem (Elem "roledelta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "roledelta" (toAttrs as) [])]
instance XmlAttributes Roledelta where
    fromAttrs as =
	Roledelta
	  { roledeltaOldrole = definiteA fromAttrToStr "roledelta" "oldrole" as
	  , roledeltaNewrole = definiteA fromAttrToStr "roledelta" "newrole" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "oldrole" (roledeltaOldrole v)
	, toAttrFrStr "newrole" (roledeltaNewrole v)
	]
instance XmlContent Booldelta where
    fromElem (CElem (Elem "booldelta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "booldelta" (toAttrs as) [])]
instance XmlAttributes Booldelta where
    fromAttrs as =
	Booldelta
	  { booldeltaOldbool = definiteA fromAttrToTyp "booldelta" "oldbool" as
	  , booldeltaNewbool = definiteA fromAttrToTyp "booldelta" "newbool" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrTyp "oldbool" (booldeltaOldbool v)
	, toAttrFrTyp "newbool" (booldeltaNewbool v)
	]
instance XmlAttrType Booldelta_Oldbool where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "true" = Just Booldelta_Oldbool_True
	    translate "false" = Just Booldelta_Oldbool_False
	    translate _ = Nothing
    toAttrFrTyp n Booldelta_Oldbool_True = Just (n, str2attr "true")
    toAttrFrTyp n Booldelta_Oldbool_False = Just (n, str2attr "false")
instance XmlAttrType Booldelta_Newbool where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "true" = Just Booldelta_Newbool_True
	    translate "false" = Just Booldelta_Newbool_False
	    translate _ = Nothing
    toAttrFrTyp n Booldelta_Newbool_True = Just (n, str2attr "true")
    toAttrFrTyp n Booldelta_Newbool_False = Just (n, str2attr "false")
instance XmlContent Skip where
    fromElem (CElem (Elem "skip" [] []):rest) =
	(Just Skip, rest)
    fromElem rest = (Nothing, rest)
    toElem Skip =
	[CElem (Elem "skip" [] [])]
instance XmlContent Edgemodedelta where
    fromElem (CElem (Elem "edgemodedelta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "edgemodedelta" (toAttrs as) [])]
instance XmlAttributes Edgemodedelta where
    fromAttrs as =
	Edgemodedelta
	  { edgemodedeltaOldem = definiteA fromAttrToTyp "edgemodedelta" "oldem" as
	  , edgemodedeltaNewem = definiteA fromAttrToTyp "edgemodedelta" "newem" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrTyp "oldem" (edgemodedeltaOldem v)
	, toAttrFrTyp "newem" (edgemodedeltaNewem v)
	]
instance XmlAttrType Edgemodedelta_Oldem where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "directed" = Just Edgemodedelta_Oldem_Directed
	    translate "undirected" = Just Edgemodedelta_Oldem_Undirected
	    translate "defaultdirected" = Just Edgemodedelta_Oldem_Defaultdirected
	    translate "defaultundirected" = Just Edgemodedelta_Oldem_Defaultundirected
	    translate _ = Nothing
    toAttrFrTyp n Edgemodedelta_Oldem_Directed = Just (n, str2attr "directed")
    toAttrFrTyp n Edgemodedelta_Oldem_Undirected = Just (n, str2attr "undirected")
    toAttrFrTyp n Edgemodedelta_Oldem_Defaultdirected = Just (n, str2attr "defaultdirected")
    toAttrFrTyp n Edgemodedelta_Oldem_Defaultundirected = Just (n, str2attr "defaultundirected")
instance XmlAttrType Edgemodedelta_Newem where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "directed" = Just Edgemodedelta_Newem_Directed
	    translate "undirected" = Just Edgemodedelta_Newem_Undirected
	    translate "defaultdirected" = Just Edgemodedelta_Newem_Defaultdirected
	    translate "defaultundirected" = Just Edgemodedelta_Newem_Defaultundirected
	    translate _ = Nothing
    toAttrFrTyp n Edgemodedelta_Newem_Directed = Just (n, str2attr "directed")
    toAttrFrTyp n Edgemodedelta_Newem_Undirected = Just (n, str2attr "undirected")
    toAttrFrTyp n Edgemodedelta_Newem_Defaultdirected = Just (n, str2attr "defaultdirected")
    toAttrFrTyp n Edgemodedelta_Newem_Defaultundirected = Just (n, str2attr "defaultundirected")
instance XmlContent Nodedelta where
    fromElem (CElem (Elem "nodedelta" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (\(d,cd)->
		    (\(e,ce)->
		       (Just (Nodedelta a b c d e), rest))
		    (fromElem cd))
		 (many fromElem cc))
	      (many fromElem cb))
	   (definite fromElem "<attributedelta>" "nodedelta" ca))
	(fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Nodedelta a b c d e) =
	[CElem (Elem "nodedelta" [] (maybe [] toElem a ++
				     toElem b ++ concatMap toElem c ++
				     concatMap toElem d ++ maybe [] toElem e))]
instance XmlContent Edgedelta where
    fromElem (CElem (Elem "edgedelta" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (\(d,cd)->
		    (\(e,ce)->
		       (\(f,cf)->
			  (\(g,cg)->
			     (\(h,ch)->
				(\(i,ci)->
				   (\(j,cj)->
				      (Just (Edgedelta a b c d e f g h i
						       j), rest))
				   (fromElem ci))
				(fromElem ch))
			     (fromElem cg))
			  (fromElem cf))
		       (fromElem ce))
		    (fromElem cd))
		 (many fromElem cc))
	      (many fromElem cb))
	   (definite fromElem "<attributedelta>" "edgedelta" ca))
	(fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Edgedelta a b c d e f g h i j) =
	[CElem (Elem "edgedelta" [] (maybe [] toElem a ++
				     toElem b ++ concatMap toElem c ++
				     concatMap toElem d ++ maybe [] toElem e ++
				     toElem f ++ maybe [] toElem g ++ toElem h
				     ++ maybe [] toElem i ++
				     maybe [] toElem j))]
instance XmlContent Orderingdelta where
    fromElem (CElem (Elem "orderingdelta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "orderingdelta" (toAttrs as) [])]
instance XmlAttributes Orderingdelta where
    fromAttrs as =
	Orderingdelta
	  { orderingdeltaOldorder = definiteA fromAttrToStr "orderingdelta" "oldorder" as
	  , orderingdeltaNeworder = definiteA fromAttrToStr "orderingdelta" "neworder" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "oldorder" (orderingdeltaOldorder v)
	, toAttrFrStr "neworder" (orderingdeltaNeworder v)
	]
instance XmlContent Reldelta where
    fromElem (CElem (Elem "reldelta" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (\(d,cd)->
		    (\(e,ce)->
		       (\(f,cf)->
			  (\(g,cg)->
			     (\(h,ch)->
				(Just (Reldelta a b c d e f g h), rest))
			     (fromElem cg))
			  (fromElem cf))
		       (many fromElem ce))
		    (many fromElem cd))
		 (fromElem cc))
	      (many fromElem cb))
	   (definite fromElem "<attributedelta>" "reldelta" ca))
	(fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Reldelta a b c d e f g h) =
	[CElem (Elem "reldelta" [] (maybe [] toElem a ++
				    toElem b ++ concatMap toElem c ++ toElem d
				    ++ concatMap toElem e ++ concatMap toElem f
				    ++ maybe [] toElem g ++ maybe [] toElem h))]
instance XmlContent Relenddelta where
    fromElem (CElem (Elem "relenddelta" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (\(d,cd)->
		    (\(e,ce)->
		       (\(f,cf)->
			  (Just (Relenddelta a b c d e f), rest))
		       (fromElem ce))
		    (fromElem cd))
		 (fromElem cc))
	      (fromElem cb))
	   (fromElem ca))
	(definite fromElem "<attributedelta>" "relenddelta" c0)
    fromElem rest = (Nothing, rest)
    toElem (Relenddelta a b c d e f) =
	[CElem (Elem "relenddelta" [] (toElem a ++
				       maybe [] toElem b ++ maybe [] toElem c ++
				       maybe [] toElem d ++ toElem e ++
				       maybe [] toElem f))]
instance XmlContent Directiondelta where
    fromElem (CElem (Elem "directiondelta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "directiondelta" (toAttrs as) [])]
instance XmlAttributes Directiondelta where
    fromAttrs as =
	Directiondelta
	  { directiondeltaOlddirection = possibleA fromAttrToTyp "olddirection" as
	  , directiondeltaNewdirection = possibleA fromAttrToTyp "newdirection" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "olddirection" (directiondeltaOlddirection v)
	, maybeToAttr toAttrFrTyp "newdirection" (directiondeltaNewdirection v)
	]
instance XmlAttrType Directiondelta_Olddirection where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "in" = Just Directiondelta_Olddirection_In
	    translate "out" = Just Directiondelta_Olddirection_Out
	    translate "none" = Just Directiondelta_Olddirection_None
	    translate _ = Nothing
    toAttrFrTyp n Directiondelta_Olddirection_In = Just (n, str2attr "in")
    toAttrFrTyp n Directiondelta_Olddirection_Out = Just (n, str2attr "out")
    toAttrFrTyp n Directiondelta_Olddirection_None = Just (n, str2attr "none")
instance XmlAttrType Directiondelta_Newdirection where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "in" = Just Directiondelta_Newdirection_In
	    translate "out" = Just Directiondelta_Newdirection_Out
	    translate "none" = Just Directiondelta_Newdirection_None
	    translate _ = Nothing
    toAttrFrTyp n Directiondelta_Newdirection_In = Just (n, str2attr "in")
    toAttrFrTyp n Directiondelta_Newdirection_Out = Just (n, str2attr "out")
    toAttrFrTyp n Directiondelta_Newdirection_None = Just (n, str2attr "none")
instance XmlContent Attributedelta where
    fromElem (CElem (Elem "attributedelta" [] c0):rest) =
	(\(a,ca)->
	   (Just (Attributedelta a), rest))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Attributedelta a) =
	[CElem (Elem "attributedelta" [] (concatMap toElem a))]
instance XmlContent Attributedelta_ where
    fromElem c0 =
	case (fromElem c0) of
	(Just a,rest) -> (Just (Attributedelta_Reduce a), rest)
	(Nothing,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Attributedelta_Change a), rest)
		(Nothing,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Attributedelta_Extend a), rest)
			(Nothing,_) ->
			    (Nothing, c0)
    fromElem rest = (Nothing, rest)
    toElem (Attributedelta_Reduce a) = toElem a
    toElem (Attributedelta_Change a) = toElem a
    toElem (Attributedelta_Extend a) = toElem a
instance XmlContent Reduce where
    fromElem (CElem (Elem "reduce" as c0):rest) =
	(\(a,ca)->
	   (Just (Reduce (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Reduce as a) =
	[CElem (Elem "reduce" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Reduce_Attrs where
    fromAttrs as =
	Reduce_Attrs
	  { reduceNewname = possibleA fromAttrToStr "newname" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "newname" (reduceNewname v)
	]
instance XmlContent Extend where
    fromElem (CElem (Elem "extend" as c0):rest) =
	(\(a,ca)->
	   (Just (Extend (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Extend as a) =
	[CElem (Elem "extend" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Extend_Attrs where
    fromAttrs as =
	Extend_Attrs
	  { extendOldname = possibleA fromAttrToStr "oldname" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "oldname" (extendOldname v)
	]
instance XmlContent Attrdelta where
    fromElem (CElem (Elem "attrdelta" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (\(d,cd)->
		    (\(e,ce)->
		       (Just (Attrdelta (fromAttrs as) a b c d e), rest))
		    (fromElem cd))
		 (fromElem cc))
	      (fromElem cb))
	   (fromElem ca))
	(fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Attrdelta as a b c d e) =
	[CElem (Elem "attrdelta" (toAttrs as) (maybe [] toElem a
					       ++ maybe [] toElem b ++
					       maybe [] toElem c ++
					       maybe [] toElem d ++
					       maybe [] toElem e))]
instance XmlAttributes Attrdelta_Attrs where
    fromAttrs as =
	Attrdelta_Attrs
	  { attrdeltaName = definiteA fromAttrToStr "attrdelta" "name" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "name" (attrdeltaName v)
	]
instance XmlContent Change where
    fromElem (CElem (Elem "change" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (\(d,cd)->
		    (\(e,ce)->
		       (\(f,cf)->
			  (Just (Change a b c d e f), rest))
		       (fromElem ce))
		    (fromElem cd))
		 (fromElem cc))
	      (fromElem cb))
	   (fromElem ca))
	(fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Change a b c d e f) =
	[CElem (Elem "change" [] (maybe [] toElem a ++
				  maybe [] toElem b ++ maybe [] toElem c ++
				  maybe [] toElem d ++ maybe [] toElem e ++
				  maybe [] toElem f))]
instance XmlContent Kinddelta where
    fromElem (CElem (Elem "kinddelta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "kinddelta" (toAttrs as) [])]
instance XmlAttributes Kinddelta where
    fromAttrs as =
	Kinddelta
	  { kinddeltaOldkind = possibleA fromAttrToStr "oldkind" as
	  , kinddeltaNewkind = possibleA fromAttrToStr "newkind" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "oldkind" (kinddeltaOldkind v)
	, maybeToAttr toAttrFrStr "newkind" (kinddeltaNewkind v)
	]
instance XmlContent Valuedelta where
    fromElem (CElem (Elem "valuedelta" [] c0):rest) =
	case (fromElem c0) of
	(Just a,_) -> (Just (ValuedeltaNewvalue a), rest)
	(Nothing,_) ->
		case (fromElem c0) of
		(Just a,_) -> (Just (ValuedeltaLocdelta a), rest)
		(Nothing,_) ->
			case (fromElem c0) of
			(Just a,_) -> (Just (ValuedeltaBooldelta a), rest)
			(Nothing,_) ->
				case (fromElem c0) of
				(Just a,_) -> (Just (ValuedeltaIntdelta a), rest)
				(Nothing,_) ->
					case (fromElem c0) of
					(Just a,_) -> (Just (ValuedeltaFloatdelta a), rest)
					(Nothing,_) ->
						case (fromElem c0) of
						(Just a,_) -> (Just (ValuedeltaStringdelta a), rest)
						(Nothing,_) ->
							case (fromElem c0) of
							(Just a,_) -> (Just (ValuedeltaAltdelta a), rest)
							(Nothing,_) ->
								case (fromElem c0) of
								(Just a,_) -> (Just (ValuedeltaGraphdelta a), rest)
								(Nothing,_) ->
								    (Nothing, c0)
    fromElem rest = (Nothing, rest)
    toElem (ValuedeltaNewvalue a) = [CElem (Elem "valuedelta" [] (toElem a) )]
    toElem (ValuedeltaLocdelta a) = [CElem (Elem "valuedelta" [] (toElem a) )]
    toElem (ValuedeltaBooldelta a) = [CElem (Elem "valuedelta" [] (toElem a) )]
    toElem (ValuedeltaIntdelta a) = [CElem (Elem "valuedelta" [] (toElem a) )]
    toElem (ValuedeltaFloatdelta a) = [CElem (Elem "valuedelta" [] (toElem a) )]
    toElem (ValuedeltaStringdelta a) = [CElem (Elem "valuedelta" [] (toElem a) )]
    toElem (ValuedeltaAltdelta a) = [CElem (Elem "valuedelta" [] (toElem a) )]
    toElem (ValuedeltaGraphdelta a) = [CElem (Elem "valuedelta" [] (toElem a) )]
instance XmlContent Newvalue where
    fromElem (CElem (Elem "newvalue" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Newvalue a b), rest))
	   (definite fromElem "<value>" "newvalue" ca))
	(definite fromElem "<value>" "newvalue" c0)
    fromElem rest = (Nothing, rest)
    toElem (Newvalue a b) =
	[CElem (Elem "newvalue" [] (toElem a ++ toElem b))]
instance XmlContent Value where
    fromElem (CElem (Elem "value" [] c0):rest) =
	case (fromElem c0) of
	(Just a,_) -> (Just (ValueLocdelta a), rest)
	(Nothing,_) ->
		case (fromElem c0) of
		(Just a,_) -> (Just (ValueBooldelta a), rest)
		(Nothing,_) ->
			case (fromElem c0) of
			(Just a,_) -> (Just (ValueIntdelta a), rest)
			(Nothing,_) ->
				case (fromElem c0) of
				(Just a,_) -> (Just (ValueFloatdelta a), rest)
				(Nothing,_) ->
					case (fromElem c0) of
					(Just a,_) -> (Just (ValueStringdelta a), rest)
					(Nothing,_) ->
						case (fromElem c0) of
						(Just a,_) -> (Just (ValueAltdelta a), rest)
						(Nothing,_) ->
							case (fromElem c0) of
							(Just a,_) -> (Just (ValueGraphdelta a), rest)
							(Nothing,_) ->
							    (Nothing, c0)
    fromElem rest = (Nothing, rest)
    toElem (ValueLocdelta a) = [CElem (Elem "value" [] (toElem a) )]
    toElem (ValueBooldelta a) = [CElem (Elem "value" [] (toElem a) )]
    toElem (ValueIntdelta a) = [CElem (Elem "value" [] (toElem a) )]
    toElem (ValueFloatdelta a) = [CElem (Elem "value" [] (toElem a) )]
    toElem (ValueStringdelta a) = [CElem (Elem "value" [] (toElem a) )]
    toElem (ValueAltdelta a) = [CElem (Elem "value" [] (toElem a) )]
    toElem (ValueGraphdelta a) = [CElem (Elem "value" [] (toElem a) )]
instance XmlContent Locdelta where
    fromElem (CElem (Elem "locdelta" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Locdelta a b), rest))
	   (fromElem ca))
	(fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Locdelta a b) =
	[CElem (Elem "locdelta" [] (maybe [] toElem a ++
				    maybe [] toElem b))]
instance XmlContent Intdelta where
    fromElem (CElem (Elem "intdelta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "intdelta" (toAttrs as) [])]
instance XmlAttributes Intdelta where
    fromAttrs as =
	Intdelta
	  { intdeltaOldint = definiteA fromAttrToStr "intdelta" "oldint" as
	  , intdeltaNewint = definiteA fromAttrToStr "intdelta" "newint" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "oldint" (intdeltaOldint v)
	, toAttrFrStr "newint" (intdeltaNewint v)
	]
instance XmlContent Floatdelta where
    fromElem (CElem (Elem "floatdelta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "floatdelta" (toAttrs as) [])]
instance XmlAttributes Floatdelta where
    fromAttrs as =
	Floatdelta
	  { floatdeltaOldfloat = definiteA fromAttrToStr "floatdelta" "oldfloat" as
	  , floatdeltaNewfloat = definiteA fromAttrToStr "floatdelta" "newfloat" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "oldfloat" (floatdeltaOldfloat v)
	, toAttrFrStr "newfloat" (floatdeltaNewfloat v)
	]
instance XmlContent Stringdelta where
    fromElem (CElem (Elem "stringdelta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "stringdelta" (toAttrs as) [])]
instance XmlAttributes Stringdelta where
    fromAttrs as =
	Stringdelta
	  { stringdeltaOldstring = definiteA fromAttrToStr "stringdelta" "oldstring" as
	  , stringdeltaNewstring = definiteA fromAttrToStr "stringdelta" "newstring" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "oldstring" (stringdeltaOldstring v)
	, toAttrFrStr "newstring" (stringdeltaNewstring v)
	]
instance XmlContent Namedelta where
    fromElem (CElem (Elem "namedelta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "namedelta" (toAttrs as) [])]
instance XmlAttributes Namedelta where
    fromAttrs as =
	Namedelta
	  { namedeltaOldname = definiteA fromAttrToStr "namedelta" "oldname" as
	  , namedeltaNewname = definiteA fromAttrToStr "namedelta" "newname" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "oldname" (namedeltaOldname v)
	, toAttrFrStr "newname" (namedeltaNewname v)
	]
instance XmlContent Altdelta where
    fromElem (CElem (Elem "altdelta" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (Just (Altdelta a b c), rest))
	      (many fromElem cb))
	   (many fromElem ca))
	(fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Altdelta a b c) =
	[CElem (Elem "altdelta" [] (maybe [] toElem a ++
				    concatMap toElem b ++ concatMap toElem c))]
instance XmlContent Altchange where
    fromElem (CElem (Elem "altchange" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "altchange" (toAttrs as) [])]
instance XmlAttributes Altchange where
    fromAttrs as =
	Altchange
	  { altchangeOldalt = definiteA fromAttrToStr "altchange" "oldalt" as
	  , altchangeNewalt = definiteA fromAttrToStr "altchange" "newalt" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "oldalt" (altchangeOldalt v)
	, toAttrFrStr "newalt" (altchangeNewalt v)
	]
instance XmlContent Accumdelta where
    fromElem (CElem (Elem "accumdelta" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Accumdelta (fromAttrs as) a b), rest))
	   (many fromElem ca))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Accumdelta as a b) =
	[CElem (Elem "accumdelta" (toAttrs as) (concatMap toElem a
						++ concatMap toElem b))]
instance XmlAttributes Accumdelta_Attrs where
    fromAttrs as =
	Accumdelta_Attrs
	  { accumdeltaAccumkind = defaultA fromAttrToTyp Accumdelta_Accumkind_Unch "accumkind" as
	  }
    toAttrs v = catMaybes 
	[ defaultToAttr toAttrFrTyp "accumkind" (accumdeltaAccumkind v)
	]
instance XmlAttrType Accumdelta_Accumkind where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "unch" = Just Accumdelta_Accumkind_Unch
	    translate "seq2set" = Just Accumdelta_Accumkind_Seq2set
	    translate "seq2bag" = Just Accumdelta_Accumkind_Seq2bag
	    translate "set2seq" = Just Accumdelta_Accumkind_Set2seq
	    translate "set2bag" = Just Accumdelta_Accumkind_Set2bag
	    translate "bag2seq" = Just Accumdelta_Accumkind_Bag2seq
	    translate "bag2set" = Just Accumdelta_Accumkind_Bag2set
	    translate _ = Nothing
    toAttrFrTyp n Accumdelta_Accumkind_Unch = Just (n, str2attr "unch")
    toAttrFrTyp n Accumdelta_Accumkind_Seq2set = Just (n, str2attr "seq2set")
    toAttrFrTyp n Accumdelta_Accumkind_Seq2bag = Just (n, str2attr "seq2bag")
    toAttrFrTyp n Accumdelta_Accumkind_Set2seq = Just (n, str2attr "set2seq")
    toAttrFrTyp n Accumdelta_Accumkind_Set2bag = Just (n, str2attr "set2bag")
    toAttrFrTyp n Accumdelta_Accumkind_Bag2seq = Just (n, str2attr "bag2seq")
    toAttrFrTyp n Accumdelta_Accumkind_Bag2set = Just (n, str2attr "bag2set")


{-Done-}

