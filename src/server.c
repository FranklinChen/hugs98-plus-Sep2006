/* --------------------------------------------------------------------------
 * Implementation of the Hugs server API.
 *
 * The Hugs server allows you to programmatically load scripts and
 * build/evaluate terms. Used by 'runhugs' to provide a batch-mode
 * UI to the interpreter.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: server.c,v $
 * $Revision: 1.45 $
 * $Date: 2005/09/10 09:42:26 $
 * ------------------------------------------------------------------------*/
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "script.h"
#include "machdep.h"
#include "evaluator.h"
#include "opts.h"
#include "strutil.h"
#include "errors.h"
#include "server.h"

#include <setjmp.h>

static Void   setHugsAPI   Args((Void));
static Bool   SetModule    Args((String));
#ifndef NO_DYNAMIC_TYPES
static Bool   linkDynamic     Args((Void));
#endif

/* --------------------------------------------------------------------------
 * Dynamic linking
 *
 * The simplest way to do dynamic linking is this:
 * 1) load the dll/shared object file
 * 2) get the address of an initialisation function
 *    from the dll symbol table
 * 3) call the initialisation function - which returns a "virtual
 *    function table" - a struct containing the addresses of all other
 *    functions and variables that we need to access.
 * ------------------------------------------------------------------------*/

static Int    GetNumScripts   Args((Void));
static Void   Reset           Args((Int));
static Void   SetOutputEnable Args((Bool));
static Void   ChangeDir       Args((String));
static Void   LoadProject     Args((String));
static Void   LoadFile        Args((String));
static Void   LoadStringF     Args((String));
static Void   SetOptions      Args((String));
static String GetOptions      Args((Void));
static HVal   CompileExpr     Args((String, String));
static Void   GarbageCollect  Args((void));
static Void   LookupName      Args((String, String));
static Void   MkInt           Args((Int));
static Void   MkAddr          Args((void*));
static Void   MkString        Args((String));
static Void   Apply           Args((Void));
static Int    EvalInt         Args((Void));
static void*  EvalAddr        Args((void));
static String EvalString      Args((Void));
static Int    DoIO            Args((Void));
static Int    DoIO_Int        Args((int*));
static Int    DoIO_Addr       Args((void**));
static HVal   PopHVal         Args((Void));
static Void   PushHVal        Args((HVal));
static Void   FreeHVal        Args((HVal));

static HugsServerAPI hugs;             /* virtual function table            */

static Void setHugsAPI() {       /* initialise virtual function table */
    static Bool api_inited = FALSE;
    if (!api_inited) {
      api_inited = TRUE;

      hugs.clearError      = ClearError;
      hugs.setHugsArgs     = setHugsArgs;
      hugs.getNumScripts   = GetNumScripts;
      hugs.reset           = Reset;
      hugs.setOutputEnable = SetOutputEnable;
      hugs.changeDir       = ChangeDir;
      hugs.loadProject     = LoadProject;
      hugs.loadFile        = LoadFile;
      hugs.loadFromBuffer  = LoadStringF;
      hugs.setOptions      = SetOptions;
      hugs.getOptions      = GetOptions;
      hugs.compileExpr     = CompileExpr;
      hugs.garbageCollect  = GarbageCollect;
      hugs.lookupName      = LookupName;
      hugs.mkInt           = MkInt;
      hugs.mkAddr          = MkAddr;
      hugs.mkString        = MkString;
      hugs.apply           = Apply;
      hugs.evalInt         = EvalInt;
      hugs.evalAddr        = EvalAddr;
      hugs.evalString      = EvalString;
      hugs.doIO            = DoIO;
      hugs.doIO_Int        = DoIO_Int;
      hugs.doIO_Addr       = DoIO_Addr;
      hugs.popHVal         = PopHVal;
      hugs.pushHVal        = PushHVal;
      hugs.freeHVal        = FreeHVal;
    }
}

/* --------------------------------------------------------------------------
 * Error handling
 *
 * We buffer error messages and refuse to execute commands until
 * the error is cleared.
 * ------------------------------------------------------------------------*/

#define ErrorBufferSize 10000

static char  serverErrMsg[ErrorBufferSize];   /* Buffer for error messages */
char* lastError = NULL;

String ClearError()
{
    String err = lastError;
    lastError  = NULL;
    ClearOutputBuffer();

    if (err && (numLoadedScripts() > 0)) 
    {
        everybody(RESET);        
        dropScriptsFrom(numLoadedScripts()-1);  /* remove partially loaded scripts */
    }
    return err;
}

Void setError(s)            /* Format an error message        */
String s; {
    Int    n = 0;
    String err = ClearOutputBuffer();

    if (NULL == err) {
	n = snprintf(serverErrMsg, ErrorBufferSize, "%s\n", s);
    } else {
	n = snprintf(serverErrMsg, ErrorBufferSize, "%s\n%s\n", s, err);
    }
    if (0 <= n && n <= ErrorBufferSize) {
	lastError = serverErrMsg;
    } else {
	lastError = "error buffer overflowed\n";
    }
}

/* All server entry points set CStackBase for the benefit of the (conservative)
 * GC and do error catching.  Any calls to Hugs functions should be "protected"
 * by being placed inside this macro.
 *
 *   void entryPoint(arg1, arg2, result)
 *   T1 arg1;
 *   T2 arg2;
 *   T3 *result;
 *   {
 *       protect(doNothing(),
 *           ...
 *       );
 *   }
 *
 * Macro decomposed into BEGIN_PROTECT and END_PROTECT pieces so that i
 * can be used on some compilers (Mac?) that have limits on the size of
 * macro arguments.
 */
#define BEGIN_PROTECT \
  if (NULL == lastError) { \
      Cell dummy; \
      CStackBase = &dummy;              /* Save stack base for use in gc  */ \
      consGC = TRUE;                    /* conservative GC is the default */ \
      if (!setjmp(catch_error)) {
#define END_PROTECT \
      } else { \
	setError("Error occurred"); \
	normalTerminal(); \
      }	\
  }
#define protect(s)	BEGIN_PROTECT s; END_PROTECT

/* --------------------------------------------------------------------------
 * Initialisation
 * ------------------------------------------------------------------------*/

/* I've added a special case for the server.  Probably should just add
   another entry point but what the heck.  If argc = -1 then the hugs
   server should NOT read registry or default hugs path stuff.  Instead,
   all options are in the first argument in argv.   -- jcp

*/

DLLEXPORT(HugsServerAPI*) initHugsServer(argc, argv) /*server initialisation*/
Int    argc;
String argv[]; {

    static Bool is_initialized = FALSE;

    if (!is_initialized) {
      is_initialized = TRUE;
      setHugsAPI();
      
      BEGIN_PROTECT			/* Too much text for protect()	   */
      Int i;

      startEvaluator();

      if (argc == -1) {
	readOptions(argv[0],FALSE);
      } else {
	readOptionSettings();
	for (i=1; i<argc && (argv[i][0]=='+' || argv[i][0]=='-'); ++i) {
	  if (!processOption(argv[i])) {
	    setError("Unrecognised option");
	    return NULL;
	  }
	}
      }
      EnableOutput(FALSE);
      loadPrelude();

#ifndef NO_DYNAMIC_TYPES
      addScriptName("Hugs.Dynamic",TRUE);
#endif

      readScripts(0);
      everybody(RESET);
#ifndef NO_DYNAMIC_TYPES
      if (!linkDynamic()) {
	setError("module Hugs.Dynamic doesn't define the expected functions");
	return NULL;
      }
#endif
      END_PROTECT
   }
   return &hugs;  /* error must have occurred */
}

/* Just give me the method table; initialisation is assumed to already have taken
 * place. Used when external code needs to callback into Haskell.
 */
HugsServerAPI* getHugsAPI() {
  setHugsAPI();
  return &hugs;
}

/* --------------------------------------------------------------------------
 * Shutting down:
 * ------------------------------------------------------------------------*/
DLLEXPORT(Void) shutdownHugsServer(hserv) /* server shutdown */
HugsServerAPI* hserv; {
  /* The 'hserv' argument isn't actually used */
  clearStack();
  stopAnyPrinting();
  everybody(EXIT);
  stopEvaluator();
  return;
}

/* --------------------------------------------------------------------------
 * Dynamic typing
 *
 * We use dynamic typing to make the system more robust - all values pushed
 * on the stack have type "Dynamic" and we check the real type on function
 * application and evaluation.  This requires on functions and a class
 * defined in the HugsDynamic library.
 *
 * sof 2001 - interposing a Dynamic-typed layer sometimes gets in the way
 *            (missing Typeable instances, extra loading of modules), as
 *            has proven the case when using the HugsServerAPI by HaskellScript,
 *            Lambada, and mod_haskell. By defining NO_DYNAMIC_TYPES,
 *            you may turn this feature off.
 * ------------------------------------------------------------------------*/
#ifndef NO_DYNAMIC_TYPES
static Name  nameRunDyn;
static Name  nameDynApp;
static Name  nameToDyn;
static Name  nameCoerceDynamic;
static Class classTypeable;

static Bool linkDynamic()
{
    nameRunDyn        = findName(findText("runDyn"));
    nameDynApp        = findName(findText("dynApp"));
    nameToDyn         = findName(findText("toDyn"));
    nameCoerceDynamic = findName(findText("coerceDynamic"));
    classTypeable     = findClass(findText("Typeable"));

    return (   nonNull(nameRunDyn        )
	    && nonNull(nameDynApp        )
	    && nonNull(nameToDyn         )
	    && nonNull(nameCoerceDynamic )
	    && nonNull(classTypeable     ));
}
#endif

/* --------------------------------------------------------------------------
 * Miscellaneous:
 * ------------------------------------------------------------------------*/

static Int GetNumScripts()      /* Get number of scripts in system  */
{
    protect(return numLoadedScripts());
    return 0;
}

static Void Reset(scripts) /* Restore number of scripts to old level */
Int scripts; {
    protect(
	ClearOutputBuffer();
	forgetScriptsFrom(scripts);
	everybody(RESET);
	);
}

static Void SetOutputEnable(f)    /* enable/disable compiler output  */
Bool f;
{
    protect(EnableOutput(f));
}

static Void ChangeDir(s)          /* change current directory        */
String s;
{
    protect(
	if (chdir(s)) {
	    setError("changeDir: invalid directory");
	    return;
	}
	);
}

static Void LoadProject(fn)       /* load a project into the system  */
String fn;
{
    protect(setError("loadProject: not implemented"));
}

static Void LoadFile(fn)          /* load a module (from a file) into the system   */
String fn;
{
    protect(
	addScriptName(fn,TRUE);
	readScripts(numLoadedScripts());
	everybody(RESET);
	);
}

static Void LoadStringF(mod)      /* load a module (from a string) into the system   */
String mod;
{
    protect(
	addScriptName(mod,TRUE);
	readScripts(numLoadedScripts());
	everybody(RESET);
	);
}

static Void SetOptions(opt)
String opt; {
  readOptions2(opt);
  return;
}

static String GetOptions() {
  return strCopy(optionsToStr());
}

static Bool SetModule(m)
String m; {
    Module mod   = findModule(findText(m));
    if (isNull(mod)) {
	return FALSE;
    }
    setCurrModule(mod);
    return TRUE;
}

static HVal CompileExpr(m,e)    /* compile expression e wrt module m */
String m;
String e; {
    BEGIN_PROTECT
	Type type = NIL;
	Cell d    = NIL;
	HVal r    = 0;

	if (!SetModule(m)) {
	    char errmsg[256];
	    snprintf(errmsg, 255, "can't find \"%s\" module", m);
	    setError(errmsg);
	    return 0;
	}
	scriptFile = 0;
	stringInput(e);                /* Put expression into input buffer */
				
	startNewScript(0);             /* Enables recovery of storage      */
				       /* allocated during evaluation      */
	parseExp();
	checkExp();
	defaultDefns = evalDefaults;
	type         = typeCheckExp(TRUE);
#ifndef NO_DYNAMIC_TYPES
	d = getTypeableDict(type);
	if (isNull(d)) {
	    setError("compileExpr: can't create Typeable instance");
	    return 0;
	}
	inputExpr = ap(ap(nameToDyn,d),inputExpr);
#endif
	compileExp();
	run(inputCode,sp);  /* Build graph for redex */

	r = mkStablePtr(pop());
	if (0 == r) {
	    setError("compileExpr: no free stable pointers");
	    return 0;
	}
	return r;
    END_PROTECT
    return 0;
}

static Void GarbageCollect()
{
    garbageCollect();
}

/* --------------------------------------------------------------------------
 * Building expressions on the stack
 * ------------------------------------------------------------------------*/

static Void LookupName(m,v) /*Push value of qualified name onto stack*/
String m,v;
{
    BEGIN_PROTECT
	Name   n;
	Cell   d;
        char   errbuf[256];

	if (!SetModule(m)) {
	    snprintf(errbuf, 255, "lookupName: invalid module '%s'", m);
	    setError(errbuf);
	    return;
	}
	if (isNull(n = findName(findText(v)))) {
            snprintf( errbuf, 255, "lookupName: invalid name '%s.%s'", m, v );
	    setError(errbuf);
	    return;
	}
#ifndef NO_DYNAMIC_TYPES
	d = getTypeableDict(name(n).type);
	if (isNull(d)) {
	    setError("lookupName: can't create Typeable instance");
	    return;
	}
	push(ap(ap(nameToDyn,d),n));
#else
	push(n);
#endif
    END_PROTECT
}

static Void MkInt(i)              /* Push an Int onto the stack      */
Int i;
{
#ifndef NO_DYNAMIC_TYPES
    Cell d = getTypeableDict(typeInt);
    protect(push(ap(ap(nameToDyn,d),mkInt(i))));
#else
    protect(push(mkInt(i)));
#endif
}

static Void MkAddr(a)              /* Push an Addr onto the stack      */
void* a;
{
#ifndef NO_DYNAMIC_TYPES
    Cell d = getTypeableDict(typeAddr);
    protect(push(ap(ap(nameToDyn,d),mkPtr(a))));
#else
    protect(push(mkPtr(a)));
#endif
}

static Void MkString(s)           /* Push a String onto the stack    */
String s;
{
    BEGIN_PROTECT
	pushString(s);
#ifndef NO_DYNAMIC_TYPES
	topfun(ap(nameToDyn,getTypeableDict(typeString)));
#endif
    END_PROTECT
}

static Void Apply()              /* Apply stack[sp-1] to stack[sp]   */
{
    BEGIN_PROTECT
	Cell x = pop();
	Cell f = pop();
#ifndef NO_DYNAMIC_TYPES
	push(ap(ap(nameDynApp,f),x));
#else
	push(ap(f,x));
#endif
    END_PROTECT
}

/* --------------------------------------------------------------------------
 * Evaluate top of stack
 * ------------------------------------------------------------------------*/

Void startEval()
{
    numCells      = 0;
    numReductions = 0;
    numGcs        = 0;
    printing      = TRUE;
    consGC = FALSE;
}

static void evalError(Cell e)
{
#define MAXLEN 255
  char message[MAXLEN] = ""; /* "error called: "; */
  int  len;
  len = strlen(message);

  if (!isAp(e) || fun(e) != nameErrorCall) {
    setError("program error");
    return;
  }
  push( arg(e) );

  while(1) {
    if (nonNull(evalWithNoError(pop())))  break; /* error in error ! */
    
    if (whnfHead!=nameCons) break;  /* end of string */
    if (nonNull(evalWithNoError(pop())))  break; /* error in error ! */

    message[len] = charOf(whnfHead);
    len++;
    if (len >= MAXLEN)      break;
  }

  message[len] = '\0';
  setError(message);
}

static Bool tryEval(Cell c)
{
    Cell temp = evalWithNoError(c);
    if (nonNull(temp))
    {
        evalError(temp);
        return FALSE;
    }
    else return TRUE;
}


Bool safeEval(Cell c)
{
        Bool ok;
        startEval();
        ok = tryEval(c);
        normalTerminal();
        return ok;
}


static Int EvalInt()            /* Evaluate a cell (:: Int)         */
{
    Cell d;
    BEGIN_PROTECT
	startEval();
#ifndef NO_DYNAMIC_TYPES
        d = getTypeableDict(typeInt);
	safeEval(ap(ap(nameCoerceDynamic,d),pop()));
#else
	safeEval(pop());
#endif
	normalTerminal();
	return whnfInt;
    END_PROTECT
    return 0;
}

static void* EvalAddr()          /* Evaluate a cell (:: Addr)         */
{
    Cell d;
    BEGIN_PROTECT
	startEval();
#ifndef NO_DYNAMIC_TYPES
        d = getTypeableDict(typeAddr);
	safeEval(ap(ap(nameCoerceDynamic,d),pop()));
#else
	safeEval(pop());
#endif
	normalTerminal();
	return ptrOf(whnfHead);
    END_PROTECT
    return 0;
}

static String EvalString()      /* Evaluate a cell (:: String)      */
{
    Cell d;
    BEGIN_PROTECT
	Int      len = 0;
	String   s;
	Bool     ok;
	StackPtr oldsp = sp;

	startEval();

	/* Evaluate spine of list onto stack */
#ifndef NO_DYNAMIC_TYPES
        d = getTypeableDict(typeString);
	ok = tryEval(ap(ap(nameCoerceDynamic,d),pop()));
#else
	ok = tryEval(pop());
#endif
        if (!ok) { sp = oldsp-1; return NULL; }

	while (whnfHead==nameCons && whnfArgs==2) {
	    Cell e  = pop();
	    Cell es = pop();
	    len++;
	    push(e);
	    ok = tryEval(es);
            if (!ok) { sp = oldsp-1; return NULL; }
	}
	normalTerminal();

	if (whnfHead != nameNil) {
	    setError("evalString: nil expected");
	    return NULL;
	}
	if (sp != oldsp-1+len) {
	    setError("evalString: unbalanced stack1");
	    return NULL;
	}

	/* Pull characters off stack into array */
	if (!(s = malloc(len+1))) {
	    setError("Malloc failed in mkString");
	    return NULL;
	}
	s[len] = '\0';
	while (--len >= 0) {
	   ok = tryEval(pop());
	   if (!ok) { sp  = oldsp; free(s); return NULL; }
	   s[len] = charOf(whnfHead);
	}
	if (sp+1 != oldsp) {
	    setError("evalString: unbalanced stack2");
	    return NULL;
	}
	return s;
    END_PROTECT
    return NULL;
}

static Int DoIO()        /* Evaluate a cell (:: IO ()) return exit status */
{
    BEGIN_PROTECT
	Int exitCode = 0;
        Bool ok;
	StackPtr oldsp = sp;
	startEval();
#ifndef NO_DYNAMIC_TYPES
	ok = safeEval(ap(nameIORun,ap(nameRunDyn, pop())));
#else
	ok = safeEval(ap(nameIORun,pop()));
#endif
        if (!ok)
        {
            sp = oldsp-1;   
            exitCode = 1;
        } else if (whnfHead == nameLeft) { /* Left exitCode -> return exitCode */
	    safeEval(pop());
	    exitCode = whnfInt;
	} else {                    /* Right void    -> return 0        */
	    drop();
	    exitCode = 0; /* implicit exit code is 0 */
	}
	normalTerminal();
	if (sp != oldsp-1) {
	    setError("doIO: unbalanced stack");
	    return 1;
	}
	return exitCode;
    END_PROTECT
    return -1; /* error code */
}

/* 
 * Evaluate a cell (:: IO Int) return exit status
 */
static Int DoIO_Int(int* phval)
{
    BEGIN_PROTECT
        Int exitCode = 0;
        Bool ok = TRUE;
        StackPtr oldsp = sp;
        startEval();
#ifndef NO_DYNAMIC_TYPES
        ok = safeEval(ap(nameIORun,ap(nameRunDyn,pop())));
#else
        ok = safeEval(ap(nameIORun,pop()));
#endif
        if (!ok)
        {
            sp = oldsp-1;   
            exitCode = 1;
        } else if (whnfHead == nameLeft) { 
            safeEval(pop());
            exitCode = whnfInt;
        } else {   
	    if (phval) {
	      safeEval(pop());
	      *phval = whnfInt;
	    } else {
	      drop();
	    }
            exitCode = 0; 
        }
        normalTerminal();
        if (sp != oldsp-1) {
            setError("doIO: unbalanced stack");
            return 1;
        }
        return exitCode;
    END_PROTECT;
    return -1; /* error code */
}

/* 
 * Evaluate a cell (:: IO Addr) return exit status
 */
static Int DoIO_Addr(void** phval)
{
    BEGIN_PROTECT
        Int exitCode = 0;
        Bool ok;
        StackPtr oldsp = sp;
        startEval();
#ifndef NO_DYNAMIC_TYPES
        ok = safeEval(ap(nameIORun,ap(nameRunDyn,pop())));
#else
        ok = safeEval(ap(nameIORun,pop()));
#endif
        if (!ok)
        {
            sp = oldsp-1;   
            exitCode = 1;
	} else if (whnfHead == nameLeft) { 
            safeEval(pop());
            exitCode = whnfInt;
        } else {   
	    if (phval) {
	      safeEval(pop());
	      *phval = (void*)ptrOf(whnfHead);
	    } else {
	      drop();
	    }
            exitCode = 0; 
        }
        normalTerminal();
        if (sp != oldsp-1) {
            setError("doIO: unbalanced stack");
            return 1;
        }
        return exitCode;
    END_PROTECT;
    return -1; /* error code */
}

/* --------------------------------------------------------------------------
 * Stable pointers
 *
 * If a value is popped off the stack, it is made into a stable pointer
 * which must be explicitly freed.
 * ------------------------------------------------------------------------*/

static HVal PopHVal()           /* Get a value off the stack         */
{
    protect(
	HVal r = mkStablePtr(pop());
	if (0 == r) {
	    setError("popHVal: no free stable pointers");
	    return 0;
	}
	return r;
	);
    return 0;
}

static Void PushHVal(hval)      /* Put a value back on the stack     */
HVal hval;
{
    protect(
	if (hval == 0) {
	    setError("pushHVal: invalid HVal");
	    return;
	}
	push(derefStablePtr(hval))
	);
}

static Void FreeHVal(hval)      /* Free a Haskell value              */
HVal   hval;
{
    protect(freeStablePtr(hval));
}
#ifndef NO_DYNAMIC_TYPES
/* --------------------------------------------------------------------------
 * Testing for class membership:
 * ------------------------------------------------------------------------*/

Cell getTypeableDict(t) /* Find a Typeable dictionary for instance t, */
Type  t; {              /* or NIL if none found                       */
    Class c = classTypeable;
    Kinds ks = NIL;
    if (isPolyType(t)) {
	ks = polySigOf(t);
	t  = monotypeOf(t);
    }
    switch (whatIs(t)) {
	case QUAL  :
	case RANK2 :
	case EXIST :
	case CDICTS: return NIL;
    }
    return provePred(ks,NIL,ap(c,t));
}
#endif

/* ----------------------------------------------------------------------- */
