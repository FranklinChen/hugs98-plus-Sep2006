/* --------------------------------------------------------------------------
 * Connections between components of the Hugs system
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: connect.h,v $
 * $Revision: 1.92 $
 * $Date: 2005/12/02 12:42:27 $
 * ------------------------------------------------------------------------*/
#ifndef __CONNECT_H__
#define __CONNECT_H__

/* --------------------------------------------------------------------------
 * Standard data:
 * ------------------------------------------------------------------------*/

extern Bool   haskell98;		/* TRUE => Haskell 98 compatibility*/
extern Module modulePrelude;
extern Module moduleUserPrelude;

/* --------------------------------------------------------------------------
 * Primitive constructor functions 
 * ------------------------------------------------------------------------*/

extern Name  nameFalse, nameTrue;
extern Name  nameNil,   nameCons;
extern Name  nameJust,	nameNothing;
extern Name  nameLeft,	nameRight;
extern Name  nameUnit;

extern Name  nameLT,      nameEQ;
extern Name  nameGT;
extern Name  nameFst,     nameSnd;      /* standard combinators		   */
extern Name  nameId,	  nameOtherwise;
extern Name  nameNegate,  nameFlip;	/* primitives reqd for parsing	   */
extern Name  nameFrom,    nameFromThen;
extern Name  nameFromTo,  nameFromThenTo;
extern Name  nameFatbar,  nameFail;	/* primitives reqd for translation */
extern Name  nameIf,	  nameSel;
extern Name  nameCompAux;
extern Name  namePmInt,	  namePmFlt;	/* primitives for pattern matching */
extern Name  namePmInteger;
#if NPLUSK
extern Name  namePmNpk,	  namePmSub;	/* primitives for (n+k) patterns   */
#endif
extern Name  nameBlackHole;		/* For GC-detected black hole	   */
extern Name  nameInd;			/* For dict indirection		   */
extern Name  nameAnd,	  nameOr;	/* For optimisation of && and ||   */
extern Name  nameFromInt, nameFromDouble;/*coercion of numerics		   */
extern Name  nameFromInteger;
extern Name  nameEq,	  nameCompare;	/* names used for deriving	   */
extern Name  nameMinBnd,  nameMaxBnd;
extern Name  nameIndex,	  nameInRange;
extern Name  nameRange;
extern Name  nameLe,      nameGt;
extern Name  nameShowsPrec, nameReadsPrec;
extern Name  nameMult,	  namePlus;
extern Name  nameConCmp,  nameEnRange;
extern Name  nameEnIndex, nameEnInRng;
extern Name  nameEnToEn,  nameEnFrEn;
extern Name  nameEnFrom,  nameEnFrTh;
extern Name  nameEnFrTo;
extern Name  nameComp,	  nameApp;	/* composition and append	   */
extern Name  nameShowField;		/* display single field		   */
extern Name  nameShowParen;		/* wrap with parens		   */
extern Name  nameReadField;		/* read single field		   */
extern Name  nameReadParen;             /* unwrap from parens              */
extern Name  nameLex;                   /* lexer                           */
extern Name  nameRangeSize;		/* calculate size of index range   */
extern Class classMonad;		/* Monads			   */
extern Name  nameReturn, nameBind;	/* for translating do / monad comps*/
extern Name  nameThen;                  /* for translating do / monad comps*/
extern Name  nameMFail;
extern Name  nameListMonad;		/* builder function for List Monad */

#if MUDO
extern Class classMonadRec;		/* Recursive monads		   */
extern Name  nameMFix;			/* for translating mdo-notation	   */
#endif

extern Name  namePrint;			/* printing primitive		   */
extern Name  nameNPrint;		/* internal printer                */

#if    IO_MONAD
extern Type   typeIO;		        /* For the IO monad, IO 	   */
extern Type   typeProgIO;		/* For the IO monad, IO ()	   */
extern Name   nameIORun;	        /* IO monad executor		   */
extern Name   nameIOBind;	        /* IO bind executor		   */
extern Name   namePutStr;	        /* Prelude.putStr                  */

extern Name   nameIOError, nameUserErr; /* primitives required for IOError */
extern Name   namePermDenied;
extern Name   nameAlreadyExists, nameAlreadyInUse, nameDoesNotExist, nameIsFull;
extern Name   nameIllegal;
#endif

#define IOArity		1		/* arity of IO actions             */

#if IO_HANDLES
extern Name   nameEOFErr;
extern Name   nameProtocolError;
#endif

#if DOTNET
extern Name   nameNetException;
#endif

extern Name   namePrimThrow;

extern Name   nameArithException;
extern Name   nameArrayException;
extern Name   nameErrorCall;
extern Name   nameIOException;
extern Name   nameNoMethodError;
extern Name   nameNonTermination;
extern Name   namePatternMatchFail;
extern Name   nameRecConError;
extern Name   nameRecSelError;
extern Name   nameRecUpdError;

extern Name   nameOverflow;
extern Name   nameDivideByZero;
extern Name   nameIndexOutOfBounds;
extern Name   nameUndefinedElement;

extern Text  textCCall;                 /* ffi tokens                      */
extern Text  textSafe;
extern Text  textUnsafe;
extern Text  textThreadsafe;
extern Text  textExport;
#if STDCALL_SUPPORTED
extern Text  textStdcall;
#endif
#ifdef DOTNET
extern Text  textDotnet;
#endif

extern Text  textPrelude;
extern Text  textUserPrelude;
extern Text  textNum;			/* used to process default decls   */
#if    NPLUSK
extern Text  textPlus;			/* Used to recognise n+k patterns  */
#endif
#if TREX
extern Name  nameNoRec;			/* The empty record		   */
extern Type  typeNoRow;			/* The empty row		   */
extern Type  typeRec;			/* Record formation		   */
extern Kind  extKind;			/* Kind of extension, *->row->row  */
extern Name  nameRecExt;		/* Extend a record		   */
extern Name  nameRecBrk;		/* Break a record		   */
extern Name  nameAddEv;			/* Addition of evidence values	   */
extern Name  nameRecSel;		/* Select a record		   */
extern Name  nameRecShw;		/* Show a record		   */
extern Name  nameShowRecRow;		/* Used to output rows		   */
extern Name  nameRecEq;			/* Compare records		   */
extern Name  nameEqRecRow;		/* Used to compare rows		   */
extern Name  nameInsFld;		/* Field insertion routine	   */
extern Name  nameShowRecRowCls;         /* Trex.ShowRecRow class           */
extern Name  nameEqRecRowCls;           /* Trex.EqRecRow class             */
#endif

extern String versionString;		/* String containing version name  */
#if USE_REGISTRY
extern String hugsRegRoot;		/* Root in registry for windows    */
#endif
extern String scriptFile;               /* Name of current script (if any) */

extern Type  typeArrow;			/* Builtin type constructors	   */
extern Type  typeList;
extern Type  typeUnit;

extern Type  typeInt;                          
extern Type  typeInt8;                          
extern Type  typeInt16;                          
extern Type  typeInt32;                          
extern Type  typeInt64;                          
extern Type  typeWord;
extern Type  typeWord8;
extern Type  typeWord16;
extern Type  typeWord32;
extern Type  typeWord64;
extern Type  typeFunPtr;
extern Type  typePtr;
extern Type  typeAddr;
extern Type  typeFloat;
extern Type  typeDouble;
extern Type  typeChar;
extern Type  typeForeignP;
extern Type  typeForeign;
extern Type  typeStable;
extern Type  typeBool;
extern Type  typeString;
#ifdef DOTNET
extern Type  typeObject;
#endif

#define fn(from,to)  ap(ap(typeArrow,from),to)	/* make type: from -> to   */

extern List  stdDefaults;		/* List of standard default types  */

extern Class classEq;			/* `standard' classes		   */
extern Class classOrd;
extern Class classShow;
extern Class classRead;
extern Class classIx;
extern Class classEnum;
extern Class classBounded;

extern Class classReal;			/* `numeric' classes		   */
extern Class classIntegral;
extern Class classRealFrac;
extern Class classRealFloat;
extern Class classFractional;
extern Class classFloating;
extern Class classNum;

extern Cell  *CStackBase;		/* pointer to base of C stack	   */

extern List  tyconDefns;		/* list of type constructor defns  */
extern List  typeInDefns;		/* list of synonym restrictions	   */
extern List  valDefns;			/* list of value definitions       */
extern List  classDefns;		/* list of class definitions       */
extern List  instDefns;			/* list of instance definitions    */
extern List  selDefns;			/* list of selector lists	   */
extern List  genDefns;			/* list of generated defns	   */
extern List  primDefns;			/* list of primitive definitions   */
extern List  unqualImports;		/* unqualified import list         */
extern List  defaultDefns;		/* default definitions (if any)	   */
extern Int   defaultLine;		/* line in which default defs occur*/
extern List  evalDefaults;		/* defaults for evaluator	   */
extern Cell  inputExpr;			/* evaluator input expression      */
extern Cell  inputContext;		/* evaluator input expression      */
extern Addr  inputCode;			/* Code for compiled input expr    */

extern Int   whnfArgs;		 	/* number of args of term in whnf  */
extern Cell  whnfHead;		 	/* head of term in whnf            */
extern Int   whnfInt;		 	/* integer value of term in whnf   */
extern Float whnfFloat;		 	/* float value of term in whnf	   */
extern Double whnfDouble;	 	/* double value of term in whnf	   */
extern Long  numReductions;		/* number of reductions used       */
extern Long  numCells;			/* number of cells allocated       */
extern Int   numGcs;			/* number of garbage collections   */
extern Bool  broken;			/* indicates interrupt received    */
extern Bool  preludeLoaded;		/* TRUE => prelude has been loaded */


/* --------------------------------------------------------------------------
 * Function prototypes etc...
 * ------------------------------------------------------------------------*/
#define RESET   1		/* reset subsystem                         */
#define MARK    2		/* mark parts of graph in use by subsystem */
#define INSTALL 3		/* install subsystem (executed once only)  */
#define EXIT	4		/* Take action immediately before exit()   */
#define BREAK   5		/* Take action after program break	   */

/* hugs.c exports: */
extern  Void   shutdownHugs     Args((Void));
extern  Void   promptForInput   Args((String));
/* The next three are required only by winhugs */
extern  Bool   doCommand        Args((Void));
extern  Void   runEditor        Args((Void));
extern  Module findEvalModule   Args((Void));

extern  Void   storage          Args((Int));

extern  Bool   startsQual        Args((Char));
extern  Void   input             Args((Int));
extern  Void   consoleInput      Args((String));
extern  Void   stringInput       Args((String));
extern  Bool   parseScript       Args((String,Long));
extern  Void   parseScriptString Args((String));
extern  Void   parseExp          Args((Void));
#if EXPLAIN_INSTANCE_RESOLUTION
extern  Void   parseContext     Args((Void));
#endif
extern  String readFilename     Args((Void));
extern  String readLine		Args((Void));
extern  Bool   isModuleId	Args((String));
extern  Syntax defaultSyntax    Args((Text));
extern  Syntax syntaxOf		Args((Name));
extern  String unlexChar        Args((Char,Char));
extern  Void   printString	Args((String));
extern  Char   getStrChr	Args((String *));

extern  Void   substitution	Args((Int));

extern  Void   staticAnalysis   Args((Int));
extern  Void   startModule      Args((Cell));
extern  Void   setExportList    Args((List));
extern  Void   setExports       Args((List));
extern  Void   tyconDefn	Args((Int,Cell,Cell,Cell));
extern  Void   setTypeIns	Args((List));
extern  Void   clearTypeIns	Args((Void));
extern  Type   fullExpand	Args((Type));
extern  Bool   isAmbiguous	Args((Type));
extern  Void   ambigError	Args((Int,String,Cell,Type));
extern  Void   classDefn	Args((Int,Cell,List,List));
extern  Void   instDefn		Args((Int,Cell,Cell));
extern  Void   addTupInst	Args((Class,Int));
extern  Bool   hasIOResultType  Args((Type));
#if TREX
extern  Inst   addRecShowInst	Args((Class,Ext));
extern  Inst   addRecEqInst	Args((Class,Ext));
#endif
#if ZIP_COMP
extern Text    zipName		Args((Int));
#endif
extern  List   oclose		Args((List,List));
extern  List   zonkTyvarsIn	Args((Type,List));
extern  Type   zonkTyvar	Args((Int));
extern  Type   zonkType		Args((Type,Int));
extern  Void   primDefn		Args((Cell,List,Cell));
extern  Void   defaultDefn	Args((Int,List));
extern  Void   checkExp		Args((Void));
#if EXPLAIN_INSTANCE_RESOLUTION
extern  Void   checkContext	Args((Void));
#endif
extern  Void   checkDefns	Args((Void));
extern  Void   h98CheckInferredType	Args((Int,Cell,Type));
extern  Void   h98DoesntSupport	Args((Int,String));
extern  Cell   depExpr          Args((Int,Cell));

extern  Void   foreignImport    Args((Cell,Cell,Cell,Cell,Cell,Type));
extern  Void   foreignExport    Args((Cell,Cell,Cell,Cell,Cell,Type));
extern  Int    foreignCount;
extern  List   foreignImports;           /* foreign import declarations     */
extern  List   foreignExports;           /* foreign export declarations     */

extern  Void   ffi              Args((Int));
extern  Void   foreignHeader    Args((String));
extern  Void   foreignFooter    Args((String,Text,List,List));
extern  Void   ffiSetFlags      Args((String));
extern  Void   ffiAddCppInclude Args((String));
extern  Void   implementForeignImport Args((Int,Name,Int,Text,Text,Bool,Text,List,Bool,Type));
extern  Void   implementForeignImportDynamic Args((Int,Int,Text,List,Bool,Type));
extern  Void   implementForeignImportWrapper Args((Int,Int,Text,List,Bool,Type));
extern  Void   implementForeignImportLabel   Args((Int,Int,Text,Text,Text,Type));
extern  Void   implementForeignExport        Args((Int,Int,Text,List,Bool,Type));
extern  Bool   foreignNeedStubs              Args((List,List));
extern  Bool   generateFFI;	/* running ffihugs? */
extern  Bool   generate_ffi;	/* generate FFI for the current module? */

extern  Void   typeChecker      Args((Int));
extern  Type   typeCheckExp	Args((Bool));
extern  Void   typeCheckDefns	Args((Void));
extern  Cell   provePred	Args((Kinds,List,Cell));
extern  Cell   resolvePred	Args((Kinds,Cell));
extern  List   simpleContext	Args((List,Int));
extern  Cell   rhsExpr		Args((Cell));
extern  Int    rhsLine		Args((Cell));
extern  Cell   getProgType	Args((List,Type));
extern  Cell   superEvid	Args((Cell,Class,Class));
extern  Void   linkPreludeTC    Args((Void));
extern  Void   linkPreludeCM    Args((Void));
extern  Void   linkPreludeFuns  Args((Void));

extern  Void   compiler         Args((Int));
extern  Void   compileDefns     Args((Void));
extern  Void   compileExp       Args((Void));
extern  Bool   failFree		Args((Cell));
extern  Int    discrArity       Args((Cell));

extern  Void   machine          Args((Int));
extern  Addr   codeGen          Args((Name,Int,Cell));
extern  Void   implementCfun	Args((Name,List));
#if TREX
extern  Name   implementRecShw  Args((Text,Cell));
extern  Name   implementRecEq   Args((Text,Cell));
#endif
extern  Void   addCfunTable	Args((Tycon));
extern  Name   succCfun		Args((Name));
extern  Name   nextCfun		Args((Name,Name));
extern  Name   cfunByNum	Args((Name,Int));
extern  Void   unwind           Args((Cell));
extern  Void   run              Args((Addr,StackPtr));
#if !WANT_FIXED_SIZE_TABLES
extern DynTable* dynMemory;
extern void    growMemory       Args((Void));
#endif

extern  Void   eval             Args((Cell));
extern  Cell   evalWithNoError  Args((Cell));
extern  Void   evalFails        Args((StackPtr));
extern  Void   throwException   Args((Cell)) HUGS_noreturn;

#if BYTECODE_PRIMS
extern Int     IntAt            Args((Addr));
extern Float   FloatAt          Args((Addr));
extern Double  DoubleAt         Args((Addr));
extern Cell    CellAt           Args((Addr));
extern Text    TextAt           Args((Addr));
extern Addr    AddrAt           Args((Addr));
extern Int     InstrAt          Args((Addr));
#endif /* BYTECODE_PRIMS */

extern  Void   builtIn          Args((Int));
extern  Void   abandon		Args((String,Cell));
extern  Void   outputString	Args((FILE *));
extern  Void   dialogue		Args((Cell));
extern  Cell   consChar		Args((Char));
#if BIGNUMS
extern  Bignum bigInt		Args((Int));
extern  Bignum bigDouble	Args((double));
extern  Bignum bigNeg		Args((Bignum));
extern  Cell   bigToInt		Args((Bignum));
extern  double bigToDouble	Args((Bignum));
extern  Bignum bigStr		Args((String));
extern  Cell   bigOut		Args((Bignum,Cell,Bool));
extern  Bignum bigShift		Args((Bignum,Int,Int));
extern  Int    bigCmp		Args((Bignum,Bignum));
#endif
#if IO_MONAD
extern Void   setHugsArgs       Args((Int,String[]));
#endif

extern  Void   machdep          Args((Int));
extern  String findPathname     Args((String));
extern  String findMPathname    Args((String));
extern  String findMInDir       Args((String,String));
extern  Bool   readable         Args((String,Bool));
#if PROFILING
extern  String timeString	Args((Void));
#endif
extern  String hugsdir          Args((Void));
extern  String mkFFIFilename    Args((String));
extern  String mkFFIFilename2   Args((String));
extern  Void   freeDLL          Args((void*));
extern  Void   compileAndLink   Args((String,String));

extern  Void   plugins          Args((Int));
extern  Bool   havePlugin       Args((String));
extern  List   calcFunDepsPreds Args((List));
extern  Inst   findInstFor      Args((Cell,Int));
#if MULTI_INST
extern  List   findInstsFor     Args((Cell,Int));
#endif

#if HUGS_FOR_WINDOWS
extern  Void saveInputState	Args((Void));
extern  Void restoreInputState	Args((Void));
#endif

#if OBSERVATIONS
#define NUMARGS         16               /* max num of args; must be 2^n    */
#define appId(seq,arg)  (seq)*NUMARGS+(arg)
#define argNum(n)       (n)%NUMARGS
#define seqNum(n)       (n)/NUMARGS
extern Bool   printingObservations;     /* TRUE => print observed exprs    */
extern Int    appNum;                   /* for counting applications       */
extern Int    obsCount;			/* sanity counter for observations */
extern Bool   isWhnf           Args((Cell));
extern Cell   getCaf           Args((Cell));
extern Int    countObserve     Args((Void));
#endif

extern  Void   charOps          Args((Int));

extern	Void   pushString	Args((String));

/*-------------------------------------------------------------------------*/

#endif /* __CONNECT_H__ */
