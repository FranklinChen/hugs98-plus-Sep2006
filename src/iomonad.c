/* --------------------------------------------------------------------------
 * Implementation of the Haskell IO monad.
 *
 * The primitives below implement the standard IO monad for Haskell 1.3
 * using a continuation passing monad for sequencing.  The primitives are
 * believed to give a reasonably good implementation of the semantics
 * specified by the Haskell 1.3 report.  There are also some additional
 * primitives, particularly for dealing with IOError and Handle values
 * that are not included in the prelude, but are used by standard libraries.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: iomonad.c,v $
 * $Revision: 1.101 $
 * $Date: 2006/05/03 09:10:40 $
 * ------------------------------------------------------------------------*/
 
Name nameIORun;			        /* run IO code                     */
Name nameIOBind;		        /* bind IO code                    */
Name namePutStr;		        /* Prelude.putStr                  */

static Name namePass;			/* auxiliary:: \f b a -> f a b     */
#if IO_HANDLES
static Name nameHreader;	        /* auxiliary function		   */
#endif
static Cell hugsProgName;		/* value of getProgName            */
static Cell hugsArgs;			/* value of getArgs                */

#if IO_HANDLES
static Void   local throwErrno     Args((String,Bool,Int,Cell *));
static String local toIOErrorDescr Args((int,Bool));
static Name   local toIOError      Args((int));
static Int    local newHandle      Args((Cell *,String));
static String local modeString     Args((Int,Bool));
static Cell   local openHandle     Args((StackPtr,Cell *,Int,Bool,String));
static Cell   local openFdHandle   Args((StackPtr,Int,Int,Bool,String));
static Char   local hGetChar       Args((Int,String));
static Void   local hPutChar       Args((Char,Int,String));
static Void   local setRWState     Args((Int,Int));
static Void   local checkOpen      Args((Int,String));
static Void   local checkReadable  Args((Int,String));
static Void   local checkWritable  Args((Int,String));
#endif

#if IO_HANDLES
# if WANT_FIXED_SIZE_TABLES
#  define MAX_HANDLES NUM_HANDLES
# else
#  define MAX_HANDLES num_handles
# endif
#endif

extern Void local pushString       Args((String));

/* --------------------------------------------------------------------------
 * IO monad control:
 * ------------------------------------------------------------------------*/

static Void iomonadControl Args((Int));
static Void iomonadControl(what)
Int what; {
    switch (what) {
	case INSTALL : 
		       setCurrModule(modulePrelude);
#define pFun(n,s,t)    addPrim(0,n=newName(findText(s),NIL),t,modulePrelude,NIL)
		       pFun(namePass,	 "_pass",    "passIO");
#if    IO_HANDLES
		       pFun(nameHreader, "_hreader", "hreader");
#endif
#undef pFun
#define predef(nm,str) nm=newName(findText(str),NIL); name(nm).defn=PREDEFINED
		       predef(nameIORun,    "hugsIORun");
		       predef(nameIOBind,   "primbindIO");
		       predef(namePutStr,   "putStr");
#undef predef
		       break;

	case MARK    : mark(hugsProgName);
		       mark(hugsArgs);
		       break;
    }
}

PROTO_PRIM(primReturnIO);
PROTO_PRIM(primBindIO);
PROTO_PRIM(primPass);

PROTO_PRIM(primGC);
PROTO_PRIM(primGetEnv);
PROTO_PRIM(primSystem);
PROTO_PRIM(primGetRandomSeed);

PROTO_PRIM(primGetProgName);
PROTO_PRIM(primGetArgs);
PROTO_PRIM(primSetProgName);
PROTO_PRIM(primSetArgs);

#if IO_HANDLES
static Void local fopenPrim   Args((StackPtr,Bool,String));
static int local getIOMode    Args((Cell));

PROTO_PRIM(primHGetChar);
PROTO_PRIM(primHPutChar);
PROTO_PRIM(primHPutStr);
PROTO_PRIM(primHreader);
PROTO_PRIM(primHContents);
PROTO_PRIM(primOpenFile);
PROTO_PRIM(primOpenBinaryFile);
PROTO_PRIM(primStdin);
PROTO_PRIM(primStdout);
PROTO_PRIM(primStderr);
PROTO_PRIM(primOpenFd);
PROTO_PRIM(primHandleToFd);
PROTO_PRIM(primHIsEOF);
PROTO_PRIM(primHFlush);
PROTO_PRIM(primHClose);
PROTO_PRIM(primHGetPosn);
PROTO_PRIM(primHSetPosn);
PROTO_PRIM(primHSetBuffering);
PROTO_PRIM(primHGetBuffering);
PROTO_PRIM(primHSeek);
PROTO_PRIM(primHLookAhead);
PROTO_PRIM(primHIsOpen);
PROTO_PRIM(primHIsClosed);
PROTO_PRIM(primHIsReadable);
PROTO_PRIM(primHIsWritable);
PROTO_PRIM(primHIsSeekable);
PROTO_PRIM(primHFileSize);
PROTO_PRIM(primHWaitForInput);
PROTO_PRIM(primEqHandle);
PROTO_PRIM(primGetHandleNumber);

PROTO_PRIM(primHSetBinaryMode);
PROTO_PRIM(primHPutBuf);
PROTO_PRIM(primHGetBuf);

PROTO_PRIM(primHIsTerminalDevice);
PROTO_PRIM(primHGetEcho);
PROTO_PRIM(primHSetEcho);

PROTO_PRIM(primIOEql);
PROTO_PRIM(primIOHash);
#endif

#if IO_REFS
PROTO_PRIM(primNewRef);
PROTO_PRIM(primDerefRef);
PROTO_PRIM(primAssignRef);
PROTO_PRIM(primEqRef);
#endif

PROTO_PRIM(primMakeSP);
PROTO_PRIM(primDerefSP);
PROTO_PRIM(primFreeSP);
PROTO_PRIM(primCastSPToP);
PROTO_PRIM(primCastPToSP);

PROTO_PRIM(primNewFP);
PROTO_PRIM(primAddFPF);
PROTO_PRIM(primAddFPFEnv);
PROTO_PRIM(primWriteFP);
PROTO_PRIM(primEqFP);
PROTO_PRIM(primTouchFP);
PROTO_PRIM(primFPToP);

#if GC_WEAKPTRS
PROTO_PRIM(primWeakPtrEq);
PROTO_PRIM(primMkWeak);
PROTO_PRIM(primDeRefWeak);
PROTO_PRIM(primReplaceFinalizer);
PROTO_PRIM(primFinalize);
PROTO_PRIM(primRunFinalizer);
PROTO_PRIM(primFinalizerWaiting);
#endif

#if STABLE_NAMES
PROTO_PRIM(primMakeSN);
PROTO_PRIM(primDerefSN);
PROTO_PRIM(primHashSN);
PROTO_PRIM(primEqSN);
#endif

#ifdef HSCRIPT
PROTO_PRIM(primGetCurrentScript);
#endif

#ifdef DOTNET
/* These primops are remnants from the first attempt at
 * providing .NET interop for Haskell / Hugs. They've been
 * mostly superceeded by the integration of .NET interop
 * with the Haskell FFI, but we'll keep these primops around
 * for a little bit longer.
 */
EXT_PROTO_PRIM(primCreateObject);
EXT_PROTO_PRIM(primInvokeMethod);
EXT_PROTO_PRIM(primInvokeStaticMethod);
EXT_PROTO_PRIM(primNewString);
EXT_PROTO_PRIM(primToHsString);
EXT_PROTO_PRIM(primNewArgArray);
EXT_PROTO_PRIM(primSetArg);
EXT_PROTO_PRIM(primGetArg);
EXT_PROTO_PRIM(primGetField);
EXT_PROTO_PRIM(primSetField);
EXT_PROTO_PRIM(primGetStaticField);
EXT_PROTO_PRIM(primSetStaticField);
EXT_PROTO_PRIM(primIsNullPtr);
EXT_PROTO_PRIM(primMkPrimVector);
#endif

static struct primitive iomonadPrimTable[] = {
  {"primretIO",		1+IOArity, primReturnIO},
  {"primbindIO",	2+IOArity, primBindIO},
  {"passIO",		2+IOArity, primPass},

  {"primGC",	        0+IOArity, primGC},
  {"getEnv",	        1+IOArity, primGetEnv},
  {"primSystem",	1+IOArity, primSystem},
  {"getRandomSeed",	0+IOArity, primGetRandomSeed},

  {"primGetProgName",   0+IOArity, primGetProgName},
  {"primGetArgs",       0+IOArity, primGetArgs},
  {"primSetProgName",   1+IOArity, primSetProgName},
  {"primSetArgs",       1+IOArity, primSetArgs},

#if IO_HANDLES
  {"hGetChar",		1+IOArity, primHGetChar},
  {"hPutChar",		2+IOArity, primHPutChar},
  {"hPutStr",		2+IOArity, primHPutStr},
  {"hreader",		1, primHreader},
  {"hGetContents",	1+IOArity, primHContents},
  {"openFile",          2+IOArity, primOpenFile},
  {"openBinaryFile",    2+IOArity, primOpenBinaryFile},
  {"openFd",            4+IOArity, primOpenFd},
  {"handleToFd",	1+IOArity, primHandleToFd},
  {"stdin",		0, primStdin},
  {"stdout",		0, primStdout},
  {"stderr",		0, primStderr},
  {"hIsEOF",		1+IOArity, primHIsEOF},
  {"hFlush",		1+IOArity, primHFlush},
  {"hClose",		1+IOArity, primHClose},
  {"hGetPosnPrim",	1+IOArity, primHGetPosn},
  {"hSetPosnPrim",	2+IOArity, primHSetPosn},
  {"hSetBuff",          3+IOArity, primHSetBuffering},
  {"hGetBuff",          1+IOArity, primHGetBuffering},
  {"hSeekPrim",         3+IOArity, primHSeek},
  {"hLookAhead",        1+IOArity, primHLookAhead},
  {"hIsOpen",		1+IOArity, primHIsOpen},
  {"hIsClosed",		1+IOArity, primHIsClosed},
  {"hIsReadable",	1+IOArity, primHIsReadable},
  {"hIsWritable",	1+IOArity, primHIsWritable},
  {"hIsSeekable",       1+IOArity, primHIsSeekable},
  {"hFileSize",         1+IOArity, primHFileSize},
  {"hWaitForInput",     2+IOArity, primHWaitForInput},
  {"primEqHandle",	2, primEqHandle},
  {"primGetHandleNumber", 1, primGetHandleNumber},
  {"hSetBinaryMode",	2+IOArity, primHSetBinaryMode},
  {"hPutBuf",		3+IOArity, primHPutBuf},
  {"hGetBuf",		3+IOArity, primHGetBuf},
  {"hIsTerminalDevice",	1+IOArity, primHIsTerminalDevice},
  {"hGetEcho",		1+IOArity, primHGetEcho},
  {"hSetEcho",		2+IOArity, primHSetEcho},
#endif

#if IO_REFS
  {"newRef",            1+IOArity, primNewRef},
  {"getRef",		1+IOArity, primDerefRef},
  {"setRef",		2+IOArity, primAssignRef},
  {"eqRef",		2, primEqRef},
#endif

  {"makeStablePtr",	1+IOArity, primMakeSP},
  {"deRefStablePtr",	1+IOArity, primDerefSP},
  {"freeStablePtr",	1+IOArity, primFreeSP},
  {"castStablePtrToPtr",1, primCastSPToP},
  {"castPtrToStablePtr",1, primCastPToSP},

  {"writeForeignObj",	2+IOArity, primWriteFP},
  {"eqForeignObj",	2, primEqFP},

  {"newForeignPtr_",	1+IOArity, primNewFP},
  {"addForeignPtrFinalizer", 2+IOArity, primAddFPF},
  {"addForeignPtrFinalizerEnv", 3+IOArity, primAddFPFEnv},
  {"touchForeignPtr",	1+IOArity, primTouchFP},
  {"unsafeForeignPtrToPtr", 1, primFPToP},

#if GC_WEAKPTRS
  {"weakPtrEq",		2, primWeakPtrEq},
  {"mkWeak",		3+IOArity, primMkWeak},
  {"deRefWeak",		1+IOArity, primDeRefWeak},
  {"replaceFinalizer",	2+IOArity, primReplaceFinalizer},
  {"finalize",		1+IOArity, primFinalize},
  {"runFinalizer",	0+IOArity, primRunFinalizer},
  {"finalizerWaiting",	0+IOArity, primFinalizerWaiting},
#endif

#if STABLE_NAMES
  {"makeStableName",	1+IOArity, primMakeSN},
  {"deRefStableName",	1, primDerefSN},
  {"hashStableName",	1, primHashSN},
  {"eqStableName",	2, primEqSN},
#endif
  
#ifdef HSCRIPT
  {"getCurrentScript",  0+IOArity, primGetCurrentScript},
#endif

#ifdef DOTNET
  {"createObject",       2+IOArity, primCreateObject},
  {"invokeMethod",       3+IOArity, primInvokeMethod},
  {"invokeStaticMethod", 2+IOArity, primInvokeStaticMethod},
  {"newString",          1+IOArity, primNewString},
  {"toString",           1+IOArity, primToHsString},
  {"newArgArray",        1+IOArity, primNewArgArray},
  {"setArrayArg",        3+IOArity, primSetArg},
  {"getArrayArg",        2+IOArity, primGetArg},
  {"getField",           2+IOArity, primGetField},
  {"setField",           3+IOArity, primSetField},
  {"getStaticField",     2+IOArity, primGetStaticField},
  {"setStaticField",     3+IOArity, primSetStaticField},
  {"isNullPtr",          1+IOArity, primIsNullPtr},
  {"mkPrimVector",       2+IOArity, primMkPrimVector},
#endif
  {"IOEql",              2+IOArity, primIOEql},
  {"IOHash",             1+IOArity, primIOHash},

  {0,			0, 0}
};

static struct primInfo iomonadPrims = { iomonadControl, iomonadPrimTable, 0 };

/* --------------------------------------------------------------------------
 * The monad combinators:
 * ------------------------------------------------------------------------*/

primFun(primReturnIO) {			/* IO monad unit		   */
    IOReturn(IOArg(1));
}

primFun(primBindIO) {			/* IO monad bind		   */
    push(ap(namePass,primArg(2)));	/* bind 3 2 1 = 3 (pass 2 1)       */
    toparg(primArg(1));
    updapRoot(primArg(3),top());
}

primFun(primPass) {			/* Auxiliary function		   */
    push(ap(primArg(3),primArg(1)));	/* pass 3 2 1 = 3 1 2		   */
    updapRoot(top(),primArg(2));
}

/* --------------------------------------------------------------------------
 * Handle operations:
 * ------------------------------------------------------------------------*/

#if IO_HANDLES

static
Int local newHandle(sCell,loc) /* return a free Handle or throw an IOError */
Cell   *sCell;
String loc; {
    Int i;

    for (i=0; i<(Int)MAX_HANDLES && nonNull(handles[i].hcell); ++i)
	;                                       /* Search for unused handle*/
    if (i>=(Int)MAX_HANDLES) {                  /* If at first we don't    */
	garbageCollect();                       /* succeed, garbage collect*/
	for (i=0; i<(Int)MAX_HANDLES && nonNull(handles[i].hcell); ++i)
	    ;                                   /* and try again ...       */
#if !WANT_FIXED_SIZE_TABLES
	if (i >= (Int)MAX_HANDLES) {
	    Int j;
	    growDynTable(dynTabHandles);
	    handles = (struct strHandle*)(dynTabHandles->data);
	    num_handles = dynTabHandles->maxIdx;
	    /* Nil out the new entries in the table */
	    for (j=i; j < (Int)num_handles; j++) {
		handles[j].hcell = NIL;
	    }
	}
#endif
    }

    if (i>=(Int)MAX_HANDLES) {                  /* ... before we give up   */
	IOFail(mkIOError(NULL,
			 nameIllegal,
			 loc,
			 "too many handles open",
			 sCell));
    }

    return i;
}

static
String local modeString(hmode,binary) /* return mode string for f(d)open */
Int  hmode;
Bool binary; {
    if (binary) {
	return (hmode&HAPPEND)    ? "ab"  :
	       (hmode&HWRITE)     ? "wb"  :
	       (hmode&HREADWRITE) ? "rb+" :
	       (hmode&HREAD)      ? "rb"  : (String)0;
    } else {
	return (hmode&HAPPEND)    ? "a"   :
	       (hmode&HWRITE)     ? "w"   :
	       (hmode&HREADWRITE) ? "r+"  :
	       (hmode&HREAD)      ? "r"   : (String)0;
    }
}

static
Cell local openHandle(root,sCell,hmode,binary,loc) /* open handle to file named s in  */
StackPtr root;
Cell   *sCell;                                     /* the specified hmode  */
Int    hmode;
Bool   binary;
String loc; {
    Int i;
    String s = evalName(*sCell);
    String stmode = modeString(hmode,binary);

    /* openHandle() either returns a Handle or throws an IOError. */

    if (!s) {				/* check for valid name		   */
	IOFail(mkIOError(NULL,
			 nameIllegal,
			 loc,
			 "illegal file name",
			 NULL));
    }

    i = newHandle(sCell,loc);

    if (!stmode) {
	IOFail(mkIOError(NULL,
			 nameIllegal,
			 loc,
			 "illegal mode",
			 NULL));
    }
    /* prepare to open file    */
    handles[i].hfp = fopen(s,stmode);
    if (hmode==HREADWRITE && handles[i].hfp==NULL) /* try to create it */
	handles[i].hfp = fopen(s, binary ? "wb+" : "w+");
    if (!handles[i].hfp)
	throwErrno(loc, TRUE, NO_HANDLE, sCell);

    handles[i].hmode = hmode;
    handles[i].hbufMode = HUNKNOWN_BUFFERING;
    handles[i].hbufSize = (-1);
    setRWState(i, RW_NEUTRAL);
#if CHAR_ENCODING
    handles[i].hBinaryMode = binary;
    handles[i].hLookAhead = -1;
#endif
    return (handles[i].hcell = ap(HANDCELL,i));
}

static
Cell local openFdHandle(root,fd,hmode,binary,loc) /* open handle to file desc fd in  */
StackPtr root;
Int    fd;					  /* the specified hmode  */
Int    hmode;
Bool   binary;
String loc; {
    Int i = newHandle(NIL,loc);
    String stmode = modeString(hmode,binary);

    /* openFdHandle() either returns a Handle or throws an IOError. */

    if (!stmode) {
	IOFail(mkIOError(NULL,
			 nameIllegal,
			 loc,
			 "illegal mode",
			 NULL));
    }
    if (!(handles[i].hfp=fdopen(fd,stmode)))
	throwErrno(loc, TRUE, NO_HANDLE, NULL);

    handles[i].hmode = hmode;
    handles[i].hbufMode = HANDLE_NOTBUFFERED;
    handles[i].hbufSize = 0;
    setRWState(i, RW_NEUTRAL);
#if CHAR_ENCODING
    handles[i].hBinaryMode = binary;
    handles[i].hLookAhead = -1;
#endif
    return (handles[i].hcell = ap(HANDCELL,i));
}

static Char local hGetChar(Int h, String fname) {
    Int c;
#if CHAR_ENCODING
    if (handles[h].hLookAhead>=0) {
	c = handles[h].hLookAhead;
	handles[h].hLookAhead = -1;
    } else if (handles[h].hBinaryMode) {
	c = getc(handles[h].hfp);
    } else {
	c = FGetChar(handles[h].hfp);
    }
#else /* !CHAR_ENCODING */
    c = FGetChar(handles[h].hfp);
#endif
    if (c==EOF && !feof(handles[h].hfp))
	throwErrno(fname, TRUE, h, NULL);
#if CHAR_ENCODING
    else if (c==BAD_CHAR) {
	IOFail(mkIOError(&handles[h].hcell,
			 nameProtocolError,
			 fname,
			 "input contains non-character data - use binary I/O for binary data",
			 NULL));
    }
#endif /* CHAR_ENCODING */
    return c;
}

static Void local hPutChar(Char c, Int h, String fname) {
    Int retval;
#if CHAR_ENCODING
    retval = handles[h].hBinaryMode ? putc(c, handles[h].hfp) :
				      FPutChar(c, handles[h].hfp);
#else
    retval = FPutChar(c, handles[h].hfp);
#endif
    if (retval == EOF)
	throwErrno(fname, TRUE, h, NULL);
#if FLUSHEVERY
    if (h <= 2) { /* Only flush the standard handles */
	fflush(handles[h].hfp);
    }
#endif
}

/* If the stream is read-write, set the state, otherwise do nothing */
static Void local setRWState(Int h, Int newState) {
    if (handles[h].hmode&HREADWRITE) {
	/* ANSI C requires repositioning between writes and reads */
	if (newState==RW_READING) {
	    if (handles[h].hRWState==RW_WRITING) {
		fflush(handles[h].hfp);
		fseek(handles[h].hfp, 0L, SEEK_CUR);
	    }
	} else if (newState==RW_WRITING) {
	    if (handles[h].hRWState==RW_READING)
		fseek(handles[h].hfp, 0L, SEEK_CUR);
	}
	handles[h].hRWState = newState;
    }
}

/* ensure that the handle is neither closed nor semi-closed */
static Void local checkOpen(Int h, String fname) {
    Int mode = handles[h].hmode;
    if (mode==HCLOSED)
	IOFail(mkIOError(&handles[h].hcell,
			 nameIllegal,
		         fname,
			 "handle is closed",
			 NULL));
    if (mode==HSEMICLOSED)
	IOFail(mkIOError(&handles[h].hcell,
			 nameIllegal,
		         fname,
			 "handle is semi-closed",
			 NULL));
}

/* ensure that the handle is readable */
static Void local checkReadable(Int h, String fname) {
    checkOpen(h, fname);
    if (!(handles[h].hmode&(HREAD|HREADWRITE)))
	IOFail(mkIOError(&handles[h].hcell,
			 nameIllegal,
			 fname,
			 "handle is not readable",
			 NULL));
}

/* ensure that the handle is writable */
static Void local checkWritable(Int h, String fname) {
    checkOpen(h, fname);
    if (!(handles[h].hmode&(HWRITE|HAPPEND|HREADWRITE)))
	IOFail(mkIOError(&handles[h].hcell,
			 nameIllegal,
			 fname,
			 "handle is not writeable",
			 NULL));
}

/* --------------------------------------------------------------------------
 * Building strings:
 * ------------------------------------------------------------------------*/

Void pushString(s)       /* push pointer to string onto stack */
String s; {
    if (*s == '\0')
	push(nameNil);
    else {
	Cell l = ap(consChar(ExtractChar(s)),NIL);
	push(l);
	while (*s) {
	    snd(l) = ap(consChar(ExtractChar(s)),NIL);
	    l = snd(l);
	}
	snd(l) = nameNil;
    }
}

/* Helper function for constructing IOErrors (see Prelude defn of
 * IOError for explanation of what the the individual arguments
 * do.
 */
  
Cell
mkIOError(mbH, kind, loc, desc, mbF)
Cell   *mbH;	/* a Handle or NULL */
Name   kind;	/* an IOErrorType */
String loc;
String desc;
Cell   *mbF;	/* a FilePath or NULL */
{
    Cell str;
    push(nameIOError);
    toparg(mbH==NULL ? nameNothing : ap(nameJust,*mbH));
    toparg(kind);
    pushString(loc); str = pop(); toparg(str);
    pushString(desc); str = pop(); toparg(str);
    toparg(mbF==NULL ? nameNothing : ap(nameJust,*mbF));
    return pop();
}


#endif
/* --------------------------------------------------------------------------
 * IO Errors (more defined for file ops)
 * ------------------------------------------------------------------------*/

static Void local throwErrno(String fname, Bool isFile, Int h, Cell *mbF)
{
    IOFail(mkIOError(h == NO_HANDLE ? NULL : &handles[h].hcell,
		     toIOError(errno),
		     fname,
		     toIOErrorDescr(errno, isFile),
		     mbF));
}

/*
 * Map a libc error code to an IOError
 */
static Name local toIOError(errc)
int errc;
{
#if HAVE_ERRNO_H  && !(__MWERKS__ && macintosh)
    switch(errc) {

    case EEXIST:
	return nameAlreadyExists;
    case ENOENT:
    case ENOTDIR:
	return nameDoesNotExist;
    case EPERM:
    case EACCES:
	return namePermDenied;
    case ENOSPC:
    case EFBIG:
	return nameIsFull;
    default:
	return nameIllegal;
    }
#else
    return nameIllegal;
#endif
}

/*
 * Map a libc error code to an IOError descriptive string
 */
static String local toIOErrorDescr(errc,isFile)
int   errc;
Bool  isFile;
{
#if HAVE_ERRNO_H  && !(__MWERKS__ && macintosh)
    switch(errc) {

    case EEXIST:
	return (isFile ? "file already exists" : "directory already exists");
    case ENOENT:
    case ENOTDIR:
	return (isFile ? "file does not exist" : "directory does not exist");
    case EPERM:
    case EACCES:
	return ""; /* No need to replicate the info conveyed by the IOErrorKind */
    case ENOSPC:
    case EFBIG:
	return "device is full";
    default:
	return "";
    }
#else
    return "";
#endif
}

/* --------------------------------------------------------------------------
 * Misc.
 * ------------------------------------------------------------------------*/

primFun(primGC) {			/* force a GC right now            */
    garbageCollect();
    IOReturn(nameUnit);
}

#if BIGNUMS && defined HAVE_TIME_H
#include <time.h>

primFun(primGetRandomSeed) {		/* generate a random seed          */
    IOReturn(bigInt(clock()));
}

#else

primFun(primGetRandomSeed) {		/* generate a random seed          */
    ERRMSG(0) "getRandomSeed is not implemented on this architecture"
    EEND;
}

#endif
				     
primFun(primGetEnv) {                 /* primGetEnv :: String -> IO String */
    String s = evalName(IOArg(1));    /* Eval name	                   */
    String r;
    if (!s) {			      /* check for valid name		   */
	IOFail(mkIOError(NULL,
			 nameIllegal,
		         "System.getEnv",
			 "illegal environment variable name",
			 &IOArg(1)));
    }
    if ((r = getenv(s))==0) {    
	IOFail(mkIOError(NULL,
			 nameDoesNotExist,
		         "System.getEnv",
			 "environment variable not found",
			 &IOArg(1)));
    }
    pushString(r);
    IOReturn(pop());
}

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
/* If it's not defined, return it verbatim. */
# define WEXITSTATUS(stat_val) (stat_val)
#endif

primFun(primSystem) {                   /* primSystem :: String -> IO Int  */
    String s = evalName(IOArg(1));	/* Eval name	                   */
    Int r;
    if (!s) {				/* check for valid string          */
	IOFail(mkIOError(NULL,
			 nameIllegal,
		         "System.system",
			 "illegal system command string",
			 &IOArg(1)));
    }
    r = shellEsc(s, TRUE/*synchronous*/, TRUE/*use shell*/);
    IOReturn(mkInt(WEXITSTATUS(r)));
}

Void setHugsArgs(argc,argv)
Int    argc;
String argv[]; {
    int i;
    Cell str;

    hugsArgs = nameNil;
    pushString(argv[0]);
    hugsProgName = pop();
    for (i=argc-1; i>0; i--) {
	pushString(argv[i]);
	str = pop();
	hugsArgs = ap2(nameCons, str, hugsArgs);
    }
}

primFun(primGetProgName) {              /* primGetProgName :: IO String    */
    IOReturn(hugsProgName);
}

primFun(primGetArgs) {                  /* primGetArgs :: IO [String]      */
    IOReturn(hugsArgs);
}

primFun(primSetProgName) {              /* primSetProgName :: String -> IO () */
    hugsProgName = IOArg(1);
    IOReturn(nameUnit);
}

primFun(primSetArgs) {                  /* primSetArgs :: [String] -> IO () */
    hugsArgs = IOArg(1);
    IOReturn(nameUnit);
}

/* --------------------------------------------------------------------------
 * File IO
 * ------------------------------------------------------------------------*/

#if IO_HANDLES

#define HandleArg(nm,offset)  \
    eval(primArg(offset));    \
    nm = intValOf(whnfHead)

#define IOBoolResult(e)  \
    IOReturn((e)?nameTrue:nameFalse)

primFun(primHGetChar) {			/* Read character from handle	   */
    Int h;
    Int c;
    HandleArg(h,1+IOArity);

    checkReadable(h, "IO.hGetChar");
    setRWState(h, RW_READING);
    c = hGetChar(h, "IO.hGetChar");
    if (c==EOF) {
	setRWState(h, RW_NEUTRAL);
	IOFail(mkIOError(&handles[h].hcell,
			 nameEOFErr,
			 "IO.hGetChar",
			 "end of file",
			 NULL));
    }
    IOReturn(mkChar(c));
}

primFun(primHPutChar) {			/* print character on handle	   */
    Char c = 0;
    Int  h;
    HandleArg(h,2+IOArity);
    CharArg(c,1+IOArity);

    checkWritable(h, "IO.hPutChar");
    setRWState(h, RW_WRITING);
    hPutChar(c, h, "IO.hPutChar");
    IOReturn(nameUnit);
}

primFun(primHPutStr) {			/* print string on handle	   */
    Int h;
    HandleArg(h,2+IOArity);
    push(primArg(1+IOArity));
    primArg(1+IOArity) = NIL;

    checkWritable(h, "IO.hPutStr");
    setRWState(h, RW_WRITING);
    blackHoleRoot();
    eval(pop());
    while (whnfHead==nameCons) {
	eval(pop());
	hPutChar(charOf(whnfHead),h,"IO.hPutStr");
	eval(pop());
    }
#if !FLUSHEVERY
    if (h <= 2) { /* Only flush the standard handles */
	fflush(handles[h].hfp);
    }
#endif
    IOReturn(nameUnit);
}

primFun(primHreader) {			/* read String from a handle 	   */
    Int h;                              /* Handle -> String                */
    HandleArg(h,1);
    if (handles[h].hmode&HSEMICLOSED) {	/* read requires semi-closed handle*/
	Int c = hGetChar(h, "IO.getContents");
	if (c!=EOF && c>=0 && c<=MAXCHARVAL) {
	    updapRoot(consChar(c),ap(nameHreader,primArg(1)));
	    return;
	}
	clearerr(handles[h].hfp);
    }
    updateRoot(nameNil);
}

primFun(primHContents) {		/* hGetContents :: Handle -> IO Str*/
    Int h;
    HandleArg(h,1+IOArity);

    checkReadable(h, "IO.hGetContents");  /* must have readable handle     */
    setRWState(h, RW_READING);
    handles[h].hmode = HSEMICLOSED; /* semi-close handle		   */
    IOReturn(ap(nameHreader,IOArg(1)));
}

static int local getIOMode(mode)	/* From IOMode to internal form    */
Cell mode; {
    Int    m = HCLOSED;

    eval(mode);				/* Eval IOMode			   */
    if (isName(whnfHead) && isCfun(whnfHead))
	switch (cfunOf(whnfHead)) {	/* we have to use numeric consts   */
	    case 1 : m = HREAD;		/* here to avoid the need to put   */
		     break;		/* IOMode in startup environment   */
	    case 2 : m = HWRITE;
		     break;
	    case 3 : m = HAPPEND;
		     break;
	    case 4 : m = HREADWRITE;
		     break;
	}
    return m;
}

static Void local fopenPrim(root,binary,loc)/* Auxiliary function for          */
StackPtr root;                              /* opening a file                  */
Bool     binary;
String   loc; {
    Int    m;

    m = getIOMode(IOArg(1));
    if (m==HCLOSED) {			/* Only accept legal modes	   */
	IOFail(mkIOError(NULL,
			 nameIllegal,
			 loc,
			 "unknown handle mode",
			 &IOArg(2)));
    }
    IOReturn(openHandle(root,&IOArg(2),m,binary,loc));
}

primFun(primOpenFile) {			/* open handle to a text file	   */
    fopenPrim(root,FALSE,"IO.openFile");
}

primFun(primOpenBinaryFile) {		/* open handle to a binary file	   */
    fopenPrim(root,TRUE,"System.IO.openBinaryFile");
}

primFun(primStdin) {			/* Standard input handle	   */
    push(handles[HSTDIN].hcell);
}

primFun(primStdout) {			/* Standard output handle	   */
    push(handles[HSTDOUT].hcell);
}

primFun(primStderr) {			/* Standard error handle	   */
    push(handles[HSTDERR].hcell);
}

primFun(primOpenFd) {			/* open handle to file descriptor. */
    Int  m;                             /* :: Int{-Fd-} -> Bool -> IOMode -> Bool -> IO Handle */
    Int  fd;
    Bool binary;
    Bool isSock;

    IntArg(fd,4+IOArity);
    BoolArg(isSock,3+IOArity);
    BoolArg(binary,1+IOArity);

    m = getIOMode(IOArg(2));
    if (m==HCLOSED) {			/* Only accept legal modes	   */
        IOFail(mkIOError(NULL,
		         nameIllegal,
		         "openFd",
		         "unknown handle mode",
		         NULL));
    }

#if defined(_WIN32) && !cygwin32_HOST_OS
    if (isSock) {
	/* fd is a SOCKET, convert it to an FD.
	 * Note: _open_osfhandle() will fail under
	 * Win9x. ToDo: better on those plats.
	 */
	fd = _open_osfhandle(fd,
			     (m & HREAD      ? O_RDONLY : 0) |
			     (m & HWRITE     ? O_WRONLY : 0) |
			     (m & HREADWRITE ? O_RDWR : 0)   |
			     (m & HAPPEND    ? O_APPEND : 0) |
			     (binary ? O_BINARY : O_TEXT)
			    );
    }
#endif

    IOReturn(openFdHandle(root,fd,m,binary,"openFd"));
}

/* Extract the file descriptor from a Handle, discarding the Handle */
primFun(primHandleToFd) {
    Int h;
    HandleArg(h,1+IOArity);
    if (IS_STANDARD_HANDLE(h)) {
        IOFail(mkIOError(NULL,
			 nameIllegal,
		         "System.Posix.IO.handleToFd",
		         "invalid handle",
			 NULL));
    }
    checkOpen(h, "System.Posix.IO.handleToFd");
#if HAVE_DUP
    {
        Int fd = dup(fileno(handles[h].hfp));
        fclose(handles[h].hfp);
        handles[h].hfp   = 0;
        handles[h].hmode = HCLOSED;
        IOReturn(mkInt(fd));
    }
#else
    IOFail(mkIOError(&handles[h].hcell,
		     nameIllegal,
		     "System.Posix.IO.handleToFd",
		     "unsupported operation",
		     NULL));
#endif
}

primFun(primHIsEOF) {	/* Test for end of file on handle  */
                        /* :: Handle -> IO Bool */
    Int h;
    FILE* fp;
    Bool isEOF;
    HandleArg(h,1+IOArity);
    checkReadable(h, "IO.hIsEOF");
    fp = handles[h].hfp;
    isEOF = feof(fp);
    if (isEOF) { /* If the EOF flag is already signalled,
		    peeking at the next char isn't likely
		    to produce a different outcome! */
	IOBoolResult(isEOF);
#if CHAR_ENCODING
    } else if (handles[h].hLookAhead>=0) {
	IOReturn(nameFalse);
#endif
    } else {
	Int c;

	setRWState(h, RW_READING);
	c = fgetc(fp);
	isEOF = feof(fp);
	if (isEOF)
	    setRWState(h, RW_NEUTRAL);
	/* Put the char back and clear any flags. */
	ungetc(c,fp);
	clearerr(fp);
	
	IOBoolResult(isEOF);
    }
}

primFun(primHFlush) {			/* Flush handle			   */
    Int h;
    HandleArg(h,1+IOArity);
    checkWritable(h, "IO.hFlush");
    fflush(handles[h].hfp);
    IOReturn(nameUnit);
}

primFun(primHClose) {			/* Close handle                   */
    Int h;
    HandleArg(h,1+IOArity);

    /* Disallow closing any of the standard handles */
    if (!IS_STANDARD_HANDLE(h) && handles[h].hmode!=HCLOSED) {
	if (h>HSTDERR && handles[h].hfp)
	    fclose(handles[h].hfp);
	handles[h].hfp   = 0;
	handles[h].hmode = HCLOSED;
    }
    /* closing an already closed handle is the identity
       (i.e., not an error.) */
    IOReturn(nameUnit);
}

primFun(primHGetPosn) {			/* Get file position               */
    Int h;
    long pos;
    HandleArg(h,1+IOArity);
    checkOpen(h, "IO.hGetPosn");
#if HAVE_FTELL
    pos = ftell(handles[h].hfp);
    IOReturn(mkInt((Int)pos));
#else
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "IO.hGetPosn",
		     "unsupported operation",
		     NULL));
#endif
}

primFun(primHSetPosn) {			/* Set file position               */
#if HAVE_FSEEK
    long   pos = 0;
#endif
    Int    h;
    HandleArg(h,2+IOArity);
    IntArg(pos,1+IOArity);
    checkOpen(h, "IO.hSetPosn");
#if HAVE_FSEEK
    setRWState(h, RW_NEUTRAL);
    fflush(handles[h].hfp);
    if (fseek(handles[h].hfp,pos,SEEK_SET) == 0) {
	IOReturn(nameUnit);
    }
#else
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "IO.hSetPosn",
		     "unsupported operation",
		     NULL));
    }
#endif
}

primFun(primHSeek) {	/* Seek to new file posn */
                        /* :: Handle -> Int -> Int -> IO () */
    Int h;
    Int sMode;
    Int off;

    HandleArg(h,3+IOArity);
    IntArg(sMode, 2+IOArity);
    IntArg(off, 1+IOArity);

    if (sMode == 0) 
	sMode = SEEK_SET;
    else if (sMode == 1)
	sMode = SEEK_CUR;
    else
	sMode = SEEK_END;

    checkOpen(h, "IO.hSeek");
    if (fseek(handles[h].hfp,off,sMode) != 0)
	throwErrno("IO.hSeek", TRUE, h, NULL);

    setRWState(h, RW_NEUTRAL);

    IOReturn(nameUnit);
}

primFun(primHLookAhead) { /* Peek at the next char */
                          /* :: Handle -> IO Char  */
    Int h;
    Int c;
    
    HandleArg(h,1+IOArity);

    checkReadable(h, "IO.hLookAhead");
#if CHAR_ENCODING
    if (handles[h].hLookAhead>=0) {
	IOReturn(mkChar(handles[h].hLookAhead));
	return;
    }
#endif
    setRWState(h, RW_READING);
    if (feof(handles[h].hfp) ||
	(c = hGetChar(h, "IO.hLookAhead")) == EOF) {
	setRWState(h, RW_NEUTRAL);
	IOFail(mkIOError(&handles[h].hcell,
			 nameEOFErr,
			 "IO.hLookAhead",
			 "end of file",
			 NULL));
    }
#if CHAR_ENCODING
    if (handles[h].hBinaryMode)
	ungetc(c, handles[h].hfp);
    else
	handles[h].hLookAhead = c;
#else
    ungetc(c, handles[h].hfp);
#endif
    IOReturn(mkChar(c));
}

primFun(primHSetBuffering) {	/* Change a Handle's buffering */
                                /* :: Handle -> Int -> Int -> IO () */
    Int h;
    Int ty;
    Int sz;
    int rc;
    HandleArg(h,3+IOArity);
    IntArg(ty,2+IOArity);
    IntArg(sz,1+IOArity);

    checkOpen(h, "IO.hSetBuffering");
    switch(ty) {
    case 0:
	ty = _IONBF;
	handles[h].hbufMode = HANDLE_NOTBUFFERED;
	handles[h].hbufSize = 0;
	break;
    case 1:
	ty = _IOLBF;
	sz = BUFSIZ;
	handles[h].hbufMode = HANDLE_LINEBUFFERED;
	handles[h].hbufSize = 0;
	break;
    case 2:
	ty = _IOFBF;
	handles[h].hbufMode = HANDLE_BLOCKBUFFERED;
	if (sz == 0) {
	    sz=BUFSIZ;
	}
	handles[h].hbufSize = sz;
	break;
    default:
	IOFail(mkIOError(&handles[h].hcell,
			 nameIllegal,
			 "IO.hSetBuffering",
			 "illegal buffer mode",
			 NULL));
    }

    /* Change the buffering mode; setvbuf() flushes the old buffer. */
    /* Let setvbuf() allocate the buffer for us. */
    rc = setvbuf(handles[h].hfp, NULL, ty, sz);
    if (rc != 0)
	throwErrno("IO.hSetBuffering", TRUE, h, NULL);
#if HAVE_ISATTY
    if ((handles[h].hmode&(HWRITE|HAPPEND|HREADWRITE)) &&
	isatty(fileno(handles[h].hfp)))
	setBuffTerminal(fileno(handles[h].hfp), ty!=0);
#endif
    IOReturn(nameUnit);
}

primFun(primHGetBuffering) {	/* Return buffering info of a handle. */
                                /*  Handle :: IO (Int,Int)            */
    Int h;
    HandleArg(h,1+IOArity);

    checkOpen(h, "IO.hGetBuffering");
    if (handles[h].hbufMode == HUNKNOWN_BUFFERING) {
	/* figure out buffer mode and size. */
#if HAVE_ISATTY
	if (isatty (fileno(handles[h].hfp)) ) {
	    /* TTY connected handles are normally linebuffered. */
	    handles[h].hbufMode =
		(handles[h].hmode&(HWRITE|HAPPEND|HREADWRITE))==0 ||
		    getBuffTerminal(fileno(handles[h].hfp)) ?
		    HANDLE_LINEBUFFERED : HANDLE_NOTBUFFERED;
	    handles[h].hbufSize = 0;
	} else {
#endif
	    /* ..if not, block buffered. */
	    handles[h].hbufMode = HANDLE_BLOCKBUFFERED;
	    handles[h].hbufSize = BUFSIZ;
#if HAVE_ISATTY
	}
#endif
    }
    IOReturn(ap(ap(mkTuple(2),mkInt((Int)handles[h].hbufMode)),
		mkInt((Int)handles[h].hbufSize)));
}

primFun(primHIsOpen) {			/* Test is handle open             */
    Int h;
    HandleArg(h,1+IOArity);
    IOBoolResult(handles[h].hmode!=HCLOSED 
		 && handles[h].hmode!=HSEMICLOSED);
}

primFun(primHIsClosed) {		/* Test is handle closed           */
    Int h;
    HandleArg(h,1+IOArity);
    IOBoolResult(handles[h].hmode==HCLOSED);
}

primFun(primHIsReadable) {		/* Test is handle readable         */
    Int h;
    HandleArg(h,1+IOArity);
    IOBoolResult(handles[h].hmode&(HREAD|HREADWRITE));
}

primFun(primHIsWritable) {		/* Test is handle writable         */
    Int h;
    HandleArg(h,1+IOArity);
    IOBoolResult(handles[h].hmode&(HWRITE|HREADWRITE|HAPPEND));
}

#if defined(IS_WINDOWS) && !defined(S_ISREG)
#define S_ISREG(x)  ((x) & _S_IFREG)
#endif

primFun(primHIsSeekable) {		/* Test if handle is writable   */
    Int h;
    Bool okHandle;
#if HAVE_FSTAT
    struct stat sb;
#endif

    HandleArg(h,1+IOArity);

    okHandle = (handles[h].hmode&(HREAD|HWRITE|HREADWRITE|HAPPEND));
#if HAVE_FSTAT
    if (okHandle && (fstat(fileno(handles[h].hfp), &sb) == 0)) {
	okHandle = S_ISREG(sb.st_mode);
    }
    IOBoolResult(okHandle);
#else
    IOFail(mkIOError(&handles[h].hcell,
		     nameIllegal,
		     "IO.hIsSeekable",
		     "unsupported operation",
		     NULL));
#endif
}

primFun(primHFileSize) {  /* If handle points to a regular file,
			     return the size of the file   */
                          /* :: Handle -> IO Integer       */
    Int h;
#if HAVE_FSTAT
    struct stat sb;
#endif

    HandleArg(h,1+IOArity);

#if HAVE_FSTAT
    checkOpen(h, "IO.hFileSize");
    if (handles[h].hmode&(HWRITE|HREADWRITE|HAPPEND))
	fflush(handles[h].hfp);
    if (fstat(fileno(handles[h].hfp), &sb) != 0 || !S_ISREG(sb.st_mode)) {
	IOFail(mkIOError(&handles[h].hcell,
			 nameIllegal,
			 "IO.hFileSize",
			 "not a regular file",
			 NULL));
    }
    IOReturn(bigWord(sb.st_size));
#else
    IOFail(mkIOError(&handles[h].hcell,
		     nameIllegal,
		     "IO.hFileSize",
		     "unsupported operation",
		     NULL));
#endif
  }

primFun(primEqHandle) {			/* Test for handle equality        */
    Int h1, h2;
    HandleArg(h1,1);
    HandleArg(h2,2);
    BoolResult(h1==h2);
}

primFun(primGetHandleNumber) {
    Int h;
    HandleArg(h,1);
    IntResult(h);
}

primFun(primHSetBinaryMode) {
    Int h;
    Bool binary;
    HandleArg(h, 2+IOArity);
    BoolArg(binary, 1+IOArity);

    checkOpen(h, "System.IO.hSetBinaryMode");
#if defined(mingw32_HOST_OS) || defined(_MSC_VER)
    setmode(fileno(handles[h].hfp), binary ? _O_BINARY : _O_TEXT);
#endif
#if CHAR_ENCODING
    handles[h].hBinaryMode = binary;
#endif
    IOReturn(nameUnit);
}

primFun(primHPutBuf) {			/* write binary data from a buffer   */
    Int h, size;
    Pointer buf;

    HandleArg(h, 3+IOArity);
    PtrArg(buf, 2+IOArity);
    IntArg(size, 1+IOArity);

    /* argument checks */
    checkWritable(h, "System.IO.hPutBuf");
    if (size < 0) {
	IOFail(mkIOError(&handles[h].hcell,
			 nameIllegal,
			 "System.IO.hPutBuf",
			 "illegal buffer size",
			 NULL));
    }
#if CHAR_ENCODING
    if (!handles[h].hBinaryMode) {
	IOFail(mkIOError(&handles[h].hcell,
			 nameIllegal,
			 "System.IO.hPutBuf",
			 "not a binary handle",
			 NULL));
    }
#endif
    setRWState(h, RW_WRITING);

    errno = 0;
    while (size > 0) {
	size -= (Int)fwrite(buf, 1, size, handles[h].hfp);
	if (errno < 0)
	    throwErrno("System.IO.hPutBuf", TRUE, h, NULL);
    }

    IOReturn(nameUnit);
}

primFun(primHGetBuf) {			/* read binary data into a buffer   */
    Int h, size, numRead;
    Pointer buf;

    HandleArg(h, 3+IOArity);
    PtrArg(buf, 2+IOArity);
    IntArg(size, 1+IOArity);

    /* argument checks */
    checkReadable(h, "System.IO.hGetBuf");
    if (size < 0) {
	IOFail(mkIOError(&handles[h].hcell,
			 nameIllegal,
			 "System.IO.hGetBuf",
			 "illegal buffer size",
			 NULL));
    }
#if CHAR_ENCODING
    if (!handles[h].hBinaryMode) {
	IOFail(mkIOError(&handles[h].hcell,
			 nameIllegal,
			 "System.IO.hGetBuf",
			 "not a binary handle",
			 NULL));
    }
#endif
    setRWState(h, RW_READING);

    numRead = (Int)fread(buf, 1, size, handles[h].hfp);
    if (numRead < size && ferror(handles[h].hfp))
	throwErrno("System.IO.hGetBuf", TRUE, h, NULL);
    IOReturn(mkInt(numRead));
}

primFun(primHWaitForInput) { /* Check whether a character can be read
				from a handle within x msecs */
                             /* :: Handle -> Int -> IO Bool */
    Int h;
    Int msecs;
    
    HandleArg(h,2+IOArity);
    IntArg(msecs,1+IOArity);
    
#if HAVE_SELECT
    checkReadable(h, "IO.hWaitForInput");
    {
	/* Implementation is a rip-off of GHC's inputReady.c */
	int maxfd, fd;
	int ready;
	fd_set rfd;
	struct timeval tv;
	
	FD_ZERO(&rfd);
	fd = fileno(handles[h].hfp);
	FD_SET(fd, &rfd);
	
	maxfd = fd + 1;
	tv.tv_sec  = msecs / 1000;
	tv.tv_usec = msecs % 1000;
	
	while ( (ready = select(maxfd, &rfd, NULL, NULL, &tv)) < 0 ) {
	    if (errno != EINTR) {
		IOFail(mkIOError(&handles[h].hcell,
				 nameIllegal,
				 "IO.hWaitForInput",
				 "input waiting terminated by signal",
				 NULL));
	    }
	}
	IOBoolResult(ready > 0);
    }
#else
    /* For now, punt on implementing async IO under Win32 */
    /* For other platforms that don't support select() on file
       file descs, please insert code that'll work. */
    IOFail(mkIOError(&handles[h].hcell,
		     nameIllegal,
		     "IO.hWaitForInput",
		     "unsupported operation",
		     NULL));
#endif
}

primFun(primHIsTerminalDevice) { /* Does the handle refer to a terminal? */
    Int h;
    HandleArg(h, 1+IOArity);
    checkOpen(h, "System.IO.hIsTerminalDevice");
#if HAVE_ISATTY
    IOBoolResult(isatty(fileno(handles[h].hfp)));
#else
    IOBoolResult(h<=HSTDERR);
#endif
}

primFun(primHGetEcho) {
    Int h;
    Int fd;
    HandleArg(h, 1+IOArity);
    checkOpen(h, "System.IO.hGetEcho");
    fd = fileno(handles[h].hfp);
#if HAVE_ISATTY
    IOBoolResult(isatty(fd) && getEchoTerminal(fd));
#else
    IOBoolResult(FALSE);
#endif
}

primFun(primHSetEcho) {
    Int h;
    Bool echo;
    Int fd;
    HandleArg(h, 2+IOArity);
    BoolArg(echo, 1+IOArity);
    checkOpen(h, "System.IO.hSetEcho");
    fd = fileno(handles[h].hfp);
#if HAVE_ISATTY
    if (isatty(fd))
	setEchoTerminal(fd, echo);
#endif
    IOReturn(nameUnit);
}

#endif /* IO_HANDLES */

/* --------------------------------------------------------------------------
 * Mutable variables
 * ------------------------------------------------------------------------*/

#if IO_REFS

#if CHECK_TAGS
#define checkRef() if (MUTVAR != whatIs(whnfHead)) internal("Ref expected")
#else
#define checkRef() /* do nothing */
#endif

primFun(primNewRef) {			/* a -> IO (Ref a)		   */
    IOReturn(ap(MUTVAR,IOArg(1)));
}

primFun(primDerefRef) {			/* Ref a -> IO a		   */
    eval(pop());
    checkRef();
    IOReturn(snd(whnfHead));
}

primFun(primAssignRef) {		/* Ref a -> a -> IO ()		   */
    eval(IOArg(2));
    checkRef();
    snd(whnfHead) = IOArg(1);
    IOReturn(nameUnit);
}

primFun(primEqRef) {			/* Ref a -> Ref a -> Bool	   */
    eval(primArg(2));
    checkRef();
    push(whnfHead);
    eval(primArg(1));
    checkRef();
    updateRoot(pop()==whnfHead ? nameTrue : nameFalse);
}
#endif

/* --------------------------------------------------------------------------
 * Stable Pointers
 * ------------------------------------------------------------------------*/

#if CHECK_TAGS
#define checkSP() checkInt()
#else
#define checkSP() /* do nothing */
#endif

#define SPArg(nm,offset)                          \
    eval(primArg(offset));                        \
    checkSP();                                    \
    nm = (HugsStablePtr)whnfInt

/* nm should be a variable in which result is stored.
   If you use an expression, reevaluation might occur */
#define SPResult(nm)                              \
   updateRoot(mkInt((Int)(nm)))


primFun(primMakeSP) {			/* a -> IO (StablePtr a)	   */
    HugsStablePtr sp = mkStablePtr(IOArg(1));
    if (sp == 0) {
        IOFail(mkIOError(NULL,
			 nameIsFull,
			 "Foreign.makeStablePtr",
		         "too many StablePtrs",
			 NULL));
    }
    IOReturn(mkInt(sp));
}

primFun(primDerefSP) {			/* StablePtr a -> IO a   	   */
    HugsStablePtr x;
    SPArg(x,1+IOArity);
    
    IOReturn(derefStablePtr(x));
}

primFun(primFreeSP) {			/* StablePtr a -> IO ()   	   */
    HugsStablePtr x;
    SPArg(x,1+IOArity);
    freeStablePtr(x);
    IOReturn(nameUnit);
}

primFun(primCastSPToP) {		/* StablePtr a -> Ptr ()   	   */
    HugsStablePtr x;
    SPArg(x,1);
    PtrResult((Pointer)x);
}

primFun(primCastPToSP) {		/* Ptr () -> StablePtr a   	   */
    Pointer x;
    PtrArg(x,1);
    SPResult((HsStablePtr)x);
}


    
/* --------------------------------------------------------------------------
 * Foreign Objects
 * ------------------------------------------------------------------------*/

#if CHECK_TAGS
#define checkForeign() if (MPCELL != whatIs(whnfHead)) internal("ForeignObj expected")
#else
#define checkForeign() /* do nothing */
#endif

primFun(primNewFP) { /* Ptr a -> IO (ForeignPtr a) */
    Pointer addr = 0;
    eval(IOArg(1));
    addr = ptrOf(whnfHead);
    IOReturn(newMallocPtr(addr));
}

primFun(primAddFPF) { /* FunPtr (Ptr a -> IO ()) -> ForeignPtr a -> IO () */
    int mp;
    eval(IOArg(1));
    mp = mpOf(whnfHead);
    eval(IOArg(2));
    mallocPtrs[mp].finalizers = cons(whnfHead, mallocPtrs[mp].finalizers);
    IOReturn(nameUnit);
}

primFun(primAddFPFEnv) {	 /* FunPtr (Ptr env -> Ptr a -> IO ()) ->  */
    int mp;			 /*	Ptr Env -> ForeignPtr a -> IO ()   */
    eval(IOArg(1));
    mp = mpOf(whnfHead);
    eval(IOArg(2));
    push(whnfHead);
    eval(IOArg(3));
    mallocPtrs[mp].finalizers =
	cons(pair(whnfHead, pop()), mallocPtrs[mp].finalizers);
    IOReturn(nameUnit);
}

primFun(primWriteFP) {		/* ForeignPtr a -> Ptr a -> IO ()	   */
    Cell mp = NIL;
    eval(IOArg(2));
    checkForeign();
    mp = whnfHead;
    eval(IOArg(1));
    derefMP(mp) = ptrOf(whnfHead);
    IOReturn(nameUnit);
}

primFun(primEqFP) {		/* ForeignPtr a -> ForeignPtr a -> Bool    */
    eval(primArg(2));
    checkForeign();
    push(whnfHead);
    eval(primArg(1));
    checkForeign();
    updateRoot(pop()==whnfHead ? nameTrue : nameFalse);
}

primFun(primTouchFP) {		/* ForeignPtr a -> IO ()	           */
    eval(IOArg(1));
    checkForeign();
    IOReturn(nameUnit);
}

primFun(primFPToP) {		/* ForeignPtr a -> Ptr a                   */
    eval(primArg(1));
    checkForeign();
    PtrResult(derefMP(whnfHead));
}

#if STABLE_NAMES
/* --------------------------------------------------------------------------
 * Stable Names
 * ------------------------------------------------------------------------*/

primFun(primMakeSN) {		/* a -> IO (StableName a)		   */
    IOReturn(ap(STABLENAME,IOArg(1)));
}

primFun(primDerefSN) {		/* StableName a -> a			   */
    eval(primArg(1));
    updateRoot(snd(whnfHead));
}

primFun(primHashSN) {		/* StableName a -> Int			   */
    eval(primArg(1));
    updateRoot(mkInt(whnfHead));
}
primFun(primEqSN) {		/* StableName a -> StableName a -> Bool	   */
    eval(primArg(2));
    push(whnfHead);
    eval(primArg(1));
    updateRoot(pop()==whnfHead ? nameTrue : nameFalse);
}
#endif

#if GC_WEAKPTRS
/* --------------------------------------------------------------------------
 * Weak Pointers
 * ------------------------------------------------------------------------*/

#if CHECK_TAGS
#define checkWeak() if(WEAKCELL!=whatIs(whnfHead)) internal("weakPtr expected");
#else
#define checkWeak() /* do nothing */
#endif

primFun(primWeakPtrEq) {		/* Weak a -> Weak a -> Bool	   */
    eval(primArg(2));
    push(whnfHead);
    eval(primArg(1));
    updateRoot(pop()==whnfHead ? nameTrue : nameFalse);
}

primFun(primMkWeak) {			/* k -> v -> Maybe (IO ())	   */
    Cell w = NIL;			/*		    -> IO (Weak v) */
    eval(IOArg(1));
    if (whnfHead==nameJust) {		/* Look for finalizer		   */
	w = pop();
    }
    w		     = ap(NIL,ap(NIL,ap(NIL,w)));
    fst(snd(w))      = IOArg(3);
    fst(snd(snd(w))) = IOArg(2);
    liveWeakPtrs     = cons(w,liveWeakPtrs);
    fst(w)           = WEAKFIN;
    IOReturn(w);
}

primFun(primDeRefWeak) {		/* Weak v -> IO (Maybe v)	   */
    eval(IOArg(1));
    if (whatIs(whnfHead)!=WEAKFIN) {
	internal("primDeRefWeak");
    }
    if (nonNull(snd(whnfHead))) {
	IOReturn(ap(nameJust,fst(snd(snd(whnfHead)))));
    } else {
	IOReturn(nameNothing);
    }
}

primFun(primReplaceFinalizer) {		/* Weak v -> Maybe (IO ())	   */
					/*	-> IO (Maybe (IO ()))	   */
    eval(IOArg(1));			/* Grab new finalizer ...	   */
    if (whnfHead!=nameJust) {
	push(NIL);
    }
    eval(IOArg(2));			/* Get weak pointer ...		   */
    if (whatIs(whnfHead)!=WEAKFIN) {
	internal("primReplaceFinalizer");
    } else if (nonNull(snd(whnfHead))) {/* ... and replace finalizer	   */
	Cell oldfin = snd(snd(snd(whnfHead)));
	snd(snd(snd(whnfHead))) = pop();
	if (nonNull(oldfin)) {
	    IOReturn(ap(nameJust,oldfin));
	}
    }
    IOReturn(nameNothing);
}

primFun(primFinalize) {			/* Weak v -> IO ()		   */
    eval(IOArg(1));			/* Bring weak pointer to an early  */
    if (whatIs(whnfHead)!=WEAKFIN) {	/* end ...			   */
	internal("primFinalize");
    } else if (nonNull(snd(whnfHead))) {
	Cell wp = whnfHead;
	Cell vf = snd(snd(wp));
	if (isPair(vf)) {
	    if (nonNull(snd(vf))) {
		fst(vf)    = snd(vf);
		snd(vf)    = finalizers;
		finalizers = vf;
	    }
	    fst(snd(wp)) = NIL;
	    snd(snd(wp)) = NIL;
	    snd(wp)      = NIL;
	}
	liveWeakPtrs = removeCell(wp,liveWeakPtrs);
    }
    IOReturn(nameUnit);
}

primFun(primRunFinalizer) {		/* IO ()			   */
    if (isNull(finalizers)) {
	IOReturn(nameUnit);
    } else {
	updapRoot(ap(hd(finalizers),primArg(2)),primArg(1));
	finalizers = tl(finalizers);
	return;
    }
}

primFun(primFinalizerWaiting) {		/* IO Boolean			   */
  IOBoolResult(!isNull(finalizers));
}
#endif /* GC_WEAKPTRS */


#if HSCRIPT
#if EMBEDDED
extern void* getCurrentScript(void);

primFun(primGetCurrentScript) {  /* IO Int */
    IOReturn( mkInt( (int)getCurrentScript() ) );
}

#else
 
primFun(primGetCurrentScript) {  /* IO Int */
    IOReturn( mkInt( 0 ) );
}

#endif /* EMBEDDED */
#endif /* HSCRIPT */

/* --------------------------------------------------------------------------
 * Primitives for implementing disposable memo functions
 * Byron Cook -- byron@cse.ogi.edu
 *
 * IOEql :: Eval a => a -> a -> IO Bool 
 *   if argument is an Int or Char
 *   then use ==
 *   else use pointer identity
 *
 * IOHash :: Eval a => a -> IO Int
 *   if a is an Int or Char
 *   then use value cast as an Int
 *   else use pointer identity
 *
 * (Earlier versions made Float a special case too - but that's not very
 *  portable since it assumes that sizeof(FloatPro) == sizeof(Int).)
 * ------------------------------------------------------------------------*/

primFun(primIOEql) {		    /* :: Eval a => a -> a -> ST Mem Bool */
    Cell x = IOArg(1);
    Cell y = IOArg(2);
    eval(x);
    eval(y);
    x = followInd(IOArg(1));
    y = followInd(IOArg(2));

    if (whatIs(x) == whatIs(y)) {
	switch (whatIs(x)) {
	    case INTCELL   : IOBoolResult(intOf(x)==intOf(y));
	    case CHARCELL  : IOBoolResult(charOf(x)==charOf(y));
	    /* deliberate fall through to end */
	}
    }
    IOBoolResult(x==y);
}

primFun(primIOHash) {                      /* :: Eval a => a -> ST Mem Int */
    Cell x = IOArg(1);
    eval(x);
    x = followInd(IOArg(1)); 

    switch(whatIs(x)) {
	case INTCELL   : IOBoolResult(x); 
	case CHARCELL  : IOBoolResult(mkInt(charOf(x)));
    }
    IOBoolResult(mkInt((Int)x));
}

/*-------------------------------------------------------------------------*/
