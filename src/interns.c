/* --------------------------------------------------------------------------
 * Implementation of primitives which provide access to internal Hugs
 * data structures and representations from within Haskell.
 *
 * Despite appearances, the only primitives which break referential
 * transparency are the pointer equality primitives.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: interns.c,v $
 * $Revision: 1.14 $
 * $Date: 2003/11/19 21:49:48 $
 * ------------------------------------------------------------------------*/
 
/* --------------------------------------------------------------------------
 * internalPrims control
 * ------------------------------------------------------------------------*/

static Void linkInternals Args((Void));

static Name nameLeft;			/* standard data constructors      */
static Name nameRight;			

static Void local internalControl Args((Int));
static Void local internalControl(what)
Int what; {
    switch (what) {
	case INSTALL : 
#define predef(nm,str) nm=newName(findText(str),NIL); name(nm).defn=PREDEFINED
		       setCurrModule(modulePrelude);
		       predef(nameLeft,       "Left");
		       predef(nameRight,      "Right");
#undef predef
		       break;

	case RESET :   linkInternals();
		       break;
    }
}

PROTO_PRIM(primGetCell);
PROTO_PRIM(primCellPtrEq);
PROTO_PRIM(primCatchError2);
PROTO_PRIM(primClassifyCell);
PROTO_PRIM(primNameInfo);
PROTO_PRIM(primNameEq);
PROTO_PRIM(primNameString);

#if BYTECODE_PRIMS
PROTO_PRIM(primNameCode);
PROTO_PRIM(primIntAt);
PROTO_PRIM(primFloatAt);
PROTO_PRIM(primDoubleAt);
PROTO_PRIM(primCellAt);
PROTO_PRIM(primNameAt);
PROTO_PRIM(primBytecodeAt);
PROTO_PRIM(primTextAt);
PROTO_PRIM(primAddrAt);
#endif /* BYTECODE_PRIMS */

static struct primitive internalPrimTable[] = {
  {"getCell",       1, primGetCell},
  {"cellPtrEq",     2, primCellPtrEq},
  {"catchError2",   1, primCatchError2},
  {"classifyCell",  2+IOArity, primClassifyCell},
  {"nameString",    1, primNameString},
  {"nameInfo",      1, primNameInfo},
  {"nameEq",        2, primNameEq},

#if BYTECODE_PRIMS
  {"nameCode",      1, primNameCode},
  {"intAt",         1, primIntAt},
  {"floatAt",       1, primFloatAt},
  {"doubleAt",      1, primDoubleAt},
  {"cellAt",        1, primCellAt},
  {"nameAt",        1, primNameAt},
  {"bytecodeAt",    1, primBytecodeAt},
  {"textAt",        1, primTextAt},
  {"addrAt",        1, primAddrAt},
#endif /* BYTECODE_PRIMS */

  {0,		    0, 0}
};

static struct primInfo internalPrims = {internalControl, internalPrimTable, 0};

/* --------------------------------------------------------------------------
 * linkInternals
 * ------------------------------------------------------------------------*/

static Module moduleInternals;

static Name nameHugsApply;            /* data CellKind = Apply Cell [Cell] */
static Name nameHugsFun;              /*    	       | Fun   Name        */
static Name nameHugsCon;              /*    	       | Con   Name        */
static Name nameHugsTuple;            /*    	       | Tuple Int         */
static Name nameHugsInt;              /*    	       | Int   Int         */
static Name nameHugsInteger;          /*    	       | Integer Integer   */
static Name nameHugsFloat;            /*    	       | Float Float       */
static Name nameHugsDouble;           /*    	       | Double Double     */
static Name nameHugsChar;             /*    	       | Char  Char        */
static Name nameHugsPrim;             /*    	       | Prim  String      */
static Name nameHugsError;            /*    	       | Error Cell        */

static Void linkInternals() {
    String internLib = "Hugs.Internals";
    moduleInternals = findModule(findText(internLib));
    if (nonNull(moduleInternals)) {
	setCurrModule(moduleInternals);
	nameHugsApply   = findName(findText("Apply"));
	nameHugsFun     = findName(findText("Fun"));
	nameHugsCon     = findName(findText("Con"));
	nameHugsTuple   = findName(findText("Tuple"));
	nameHugsInt     = findName(findText("Int"));
	nameHugsInteger = findName(findText("Integer"));
	nameHugsFloat   = findName(findText("Float"));
	nameHugsDouble  = findName(findText("Double"));
	nameHugsChar    = findName(findText("Char"));
	nameHugsPrim    = findName(findText("Prim"));
	nameHugsError   = findName(findText("Error"));
     
	if (isNull(nameHugsApply)   
	    || isNull(nameHugsFun)     
	    || isNull(nameHugsCon)     
	    || isNull(nameHugsTuple)   
	    || isNull(nameHugsInt)     
	    || isNull(nameHugsInteger) 
	    || isNull(nameHugsFloat)   
	    || isNull(nameHugsDouble)   
	    || isNull(nameHugsChar)    
	    || isNull(nameHugsPrim)    
	    || isNull(nameHugsError)
	    ) {
		fatal("module HugsInternals doesn't define CellKind correctly");
	}
    }
}

/* --------------------------------------------------------------------------
 * Operations on Cells
 * ------------------------------------------------------------------------*/

/* Cells have to be boxed (using the HUGSOBJECT tag) so that we can        */
/* evaluate a Cell expression such as "fst (cell1, cell2)" without         */
/* evaluating the thunk inside the Cell.                                   */

#define isHugsObject(a)     (isPair(a) && fst(a)==HUGSOBJECT)
#define checkHugsObject(x)  \
  if (!isHugsObject(x)) internal("HugsObject expected")

#define HugsObjectArg(nm, offset)                  \
   eval(primArg(offset));                          \
   checkHugsObject(whnfHead);                      \
   nm = snd(whnfHead);

primFun(primGetCell) {                 /* Trivial coercion                 */
    updapRoot(HUGSOBJECT,primArg(1));  /*  :: a -> Cell                    */
}


/* --------------------------------------------------------------------------
 * Pointer equality on Cells
 * ------------------------------------------------------------------------*/

primFun(primCellPtrEq) {	       /* Unsafe pointer equality test     */
    Cell x = 0;
    Cell y = 0;
    HugsObjectArg(x,1);
    push(x);
    HugsObjectArg(y,2);
    x = pop();

    /* We chase indirection chains since that's easy and gives more
     * accurate results.  
     * Don't do this if we use this function during error recovery -
     * it might be possible for an infinite loop to occur.
     */
    while (isPair(x) && INDIRECT == fst(x))
	x = snd(x);
    while (isPair(y) && INDIRECT == fst(y))
	y = snd(y);
    updateRoot( (x==y) ? nameTrue : nameFalse );
}

/* --------------------------------------------------------------------------
 * Error catching - evaluate a Cell and return an error redex or its value
 * ------------------------------------------------------------------------*/

primFun(primCatchError2) {	       /* Error catching  primitive        */
				       /*  :: a -> Either Cell a           */
    Cell err = NIL;
    err = evalWithNoError(primArg(1)); 
    if (isNull(err)) {
	updapRoot(nameRight, primArg(1));
    } else {
	updapRoot(nameLeft, ap(HUGSOBJECT, err));
    }
}

/* --------------------------------------------------------------------------
 * Examine an object either with or without evaluation.
 *
 * This primitive lets Haskell programs examine values of any datatype
 * (probably to print them).  It is wired into the IO monad to (narrowly)
 * avoid breaking referential transparency.
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Note: we can't force the object _outside_ classify for two reasons: 
 *									     
 * 1) Writing "strict classifyCell x" only causes x to be evaluated to 
 *    WHNF.  Since x is a Cell, this doesn't achieve very much.	     
 *									     
 * 2) We can overcome this by adding an operation 			     
 *									     
 *      "forceCell :: Cell -> Cell" 			     
 *									     
 *    and writing "classifyObject (forceObject x)" but, if "x" is a CAF, 
 *    then "x" will be updated but the indirection via "x" will remain  
 *    in place.								     
 * ------------------------------------------------------------------------*/

primFun(primClassifyCell) {          /* classifyCell                       */
    Bool gc = consGC;                /*  :: Bool -> Cell -> IO CellKind    */
    Bool strict;
    Cell result = NIL;
    BoolArg(strict,2+IOArity);
    if (strict) {
	Cell temp;
	eval(IOArg(1)); /* object */
	checkHugsObject(whnfHead);
	whnfHead = snd(whnfHead);
	if (nonNull(temp = evalWithNoError(whnfHead))) {
	    IOReturn(ap(nameHugsError,ap(HUGSOBJECT,temp)));
	}
    } else {
	eval(IOArg(1)); /* object */
	checkHugsObject(whnfHead);
	whnfHead = snd(whnfHead);
	unwind(whnfHead);
    }
    consGC = TRUE;
    if (whnfArgs > 0) {
	Cell args = nameNil;
	while (whnfArgs-- > 0) 
	    args = ap(ap(nameCons,
			 ap(HUGSOBJECT, pushed(whnfArgs))),
			 args);
	result = ap(ap(nameHugsApply, 
		       ap(HUGSOBJECT,whnfHead)), 
		       args);
    } else {
	switch (whatIs(whnfHead)) {

	    case NAME     : result = ap((isCfun(whnfHead) ? nameHugsCon 
							  : nameHugsFun), 
					mkInt(whnfHead));
			    break;

	    case TUPLE    : result = ap(nameHugsTuple,mkInt(tupleOf(whnfHead)));
			    break;

#if BIGNUMS
	    case ZERONUM  : 
	    case POSNUM   : 
	    case NEGNUM   : 
			    result = ap(nameHugsInteger, whnfHead);
			    break;
#endif

	    case INTCELL  : result = ap(nameHugsInt, whnfHead);
			    break;

	    case CHARCELL : result = ap(nameHugsChar, whnfHead);
			    break;

	    case FLOATCELL: result = ap(nameHugsFloat, whnfHead);
			    break;

	    case DOUBLECELL: result = ap(nameHugsDouble, whnfHead);
			    break;

#if IO_HANDLES
	    case HANDCELL : result = ap(nameHugsPrim, mkStr(findText("{handle}")));
			    break;
#endif

#if IO_REFS
	    case MUTVAR   : result = ap(nameHugsPrim, mkStr(findText("{ref}")));
			    break;
#endif

#if HASKELL_ARRAYS
	    case ARRAY    : result = ap(nameHugsPrim, mkStr(findText("{primitive array}")));
			    break;
#endif

	    case HUGSOBJECT : 
			    /* ToDo: fix so that debugger can examine itself */
			    result = ap(nameHugsPrim, mkStr(findText("{Cell ...}")));
			    break;

	    case INSTANCE : result = ap(nameHugsPrim, mkStr(findText("{instance}")));
			    break;
#if OBSERVATIONS
            case OBSERVE  : result = ap(nameHugsPrim, mkStr(findText("{observe marker}")));
                            break;
#endif
	    default       : internal("Error in graph2");
			    break;
	}
    }
    consGC = gc;
    IOReturn(result);
}

/* --------------------------------------------------------------------------
 * Operations on Names
 * ------------------------------------------------------------------------*/

primFun(primNameString) {              /* Get string of a name             */
  Text t;                              /*  :: Name -> String              */
  Name nm;
  IntArg(nm,1);
  t = name(nm).text;
  if (0 <= t && t < NUM_TEXT) {
      /* ToDo: replace updateRoot(mkStr(..)) with updapRoot(STRCELL,..) */
      updateRoot(mkStr(t));
  } else { 
      /* If textToStr generates result on the fly, we build the entire
       * string now.
       */
      pushString(textToStr(t));
      updateRoot(pop());
  }      
}

primFun(primNameInfo) {                /* Get arity and fixity of a Name   */
  Name nm;                             /*  :: Name -> (Int,Int,Char)       */
  Syntax syn = 0;
  Char assoc = 0;  
  IntArg(nm,1);
  syn = syntaxOf(nm);
  switch (assocOf(syn)) {
  case APPLIC    : assoc = 'A'; break;
  case LEFT_ASS  : assoc = 'L'; break;
  case RIGHT_ASS : assoc = 'R'; break;
  case NON_ASS   : assoc = 'N'; break;
  }
  updapRoot(ap(ap(mkTuple(3),
	    mkInt(name(nm).arity)),
	    mkInt(precOf(syn))),
	    mkChar(assoc));
}

primFun(primNameEq) {                  /* Compare names                    */
  Name nm1;                            /*  :: Name -> Name -> Bool         */
  Name nm2;                    
  IntArg(nm1,2);
  IntArg(nm2,1);
  BoolResult(nm1==nm2);
}

#if BYTECODE_PRIMS

primFun(primNameCode) {                /* Obtain address of first bytecode */
  Name nm;                             /*  :: Name -> Addr                 */
  IntArg(nm,1);
  IntResult(name(nm).code);
}

#endif /* BYTECODE_PRIMS */

/* --------------------------------------------------------------------------
 * Operations on Addresses/Bytecodes
 * ------------------------------------------------------------------------*/

#if BYTECODE_PRIMS

primFun(primIntAt) {                  /* Obtain Int at address             */
  Addr m;                             /*  :: Addr -> Int                   */
  IntArg(m,1);
  IntResult(IntAt(m));
}

primFun(primFloatAt) {                /* Obtain float at address           */
  Addr m;                             /*  :: Addr -> Cell                  */
  IntArg(m,1);
  FloatResult(FloatAt(m));
}

primFun(primDoubleAt) {               /* Obtain float at address           */
  Addr m;                             /*  :: Addr -> Cell                  */
  IntArg(m,1);
  DoubleResult(DoubleAt(m));
}

primFun(primCellAt) {                 /* Obtain cell at address            */
  Addr m;                             /*  :: Addr -> Cell                  */
  IntArg(m,1);
  updapRoot(HUGSOBJECT,CellAt(m));
}

primFun(primNameAt) {                 /* Obtain Name at address            */
  Addr m;                             /*  :: Addr -> Name                  */
  IntArg(m,1);
  IntResult(CellAt(m));
}

primFun(primBytecodeAt) {             /* Obtain bytecode at address        */
  Addr m;                             /*  :: Addr -> Bytecode              */
  IntArg(m,1);
  IntResult(InstrAt(m));
}

primFun(primTextAt) {                 /* Obtain string at address          */
  Addr m;                             /*  :: Addr -> String                */
  IntArg(m,1);
  updapRoot(STRCELL,TextAt(m));
}

primFun(primAddrAt) {                 /* Obtain address at address         */
  Addr m;                             /*  :: Addr -> Addr                  */
  IntArg(m,1);
  IntResult(AddrAt(m));
}

#endif /* BYTECODE_PRIMS */

/*-------------------------------------------------------------------------*/
