/*
 * The Hugs evaluator / command interpreter + support functions.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 */
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "script.h"
#include "output.h"
#include "strutil.h"
#include "opts.h"
#include "machdep.h"
#include "evaluator.h"

#if HAVE_WINDOWS_H
#include <windows.h>
#endif

/* --------------------------------------------------------------------------
 * Flags and options:
 *
 * Note: definitions here are restricted to interpreter state that's independent
 *       of the UI used.
 *
 * ------------------------------------------------------------------------*/
Int    hpSize            = DEFAULTHEAP; /* Desired heap size               */
String hugsPath		 = 0;		/* String for file search path     */
String hugsSuffixes	 = 0;		/* Source filename suffixes        */

/* --------------------------------------------------------------------------
 * Evaluator initialization:
 * ------------------------------------------------------------------------*/
Void startEvaluator(Void)
{
    initScripts();

    hugsPath      = uniqPath(strCopy(HUGSPATH));
    hugsSuffixes  = strCopy(HUGSSUFFIXES);
#if HSCRIPT
    hscriptSuffixes();
#endif
}

/* --------------------------------------------------------------------------
 * Shutdown evaluator.
 * ------------------------------------------------------------------------*/
Void stopEvaluator() {
  /* Let go of dynamic storage */  
    if (hugsPath)     { free(hugsPath); hugsPath=0; }
    if (hugsSuffixes) { free(hugsSuffixes); hugsSuffixes=0; }
    if (prompt)       { free(prompt);    prompt=0; }
    if (repeatStr)    { free(repeatStr); repeatStr=0; }
}

Void evaluator(m)
Module m; {        /* evaluate expr and print value    */
    Type  type, bd, t;
    Kinds ks   = NIL;
    Cell  temp = NIL;

    setCurrModule(m);
    scriptFile = 0;
    startNewScript(0);                 /* Enables recovery of storage      */
				       /* allocated during evaluation      */
    parseExp();
    checkExp();
    defaultDefns = evalDefaults;
    type         = typeCheckExp(TRUE);
    if (isPolyType(type)) {
	ks = polySigOf(type);
	bd = monotypeOf(type);
    }
    else
	bd = type;

    if (whatIs(bd)==QUAL) {
	ERRMSG(0) "Unresolved overloading" ETHEN
	ERRTEXT   "\n*** Type       : "    ETHEN ERRTYPE(type);
	ERRTEXT   "\n*** Expression : "    ETHEN ERREXPR(inputExpr);
	ERRTEXT   "\n"
	EEND;
    }
  
#if PROFILING
    if (profiling)
	profilerLog("profile.hp");
    numReductions = 0;
    garbageCollect();
#endif

#if WANT_TIMER
    updateTimers();
#endif
#if IO_MONAD
    if ((t = getProgType(ks,type)) != 0) {
        if (displayIO) {
            Cell printer = namePrint;
            if (useShow) {
                Cell d = resolvePred(ks,ap(classShow,t));
                if (isNull(d)) {
                    printing = FALSE;
                    ERRMSG(0) "Cannot find \"show\" function for IO result:" ETHEN
                    ERRTEXT   "\n*** Expression : "   ETHEN ERREXPR(inputExpr);
                    ERRTEXT   "\n*** Of type    : "   ETHEN ERRTYPE(type);
                    ERRTEXT   "\n"
                    EEND;
                }
                printer = ap(nameShowsPrec,d);
            }
            printer = ap(ap(nameFlip,ap(printer,mkInt(MIN_PREC))),nameNil);
            printer = ap(ap(nameComp,namePutStr),printer);
            inputExpr = ap(ap(nameIOBind,inputExpr),printer);
	}
    }
    else
#endif
    {   Cell printer = namePrint;
	if (useShow) {
	    Cell d = resolvePred(ks,ap(classShow,bd));
	    if (isNull(d)) {
		printing = FALSE;
		ERRMSG(0) "Cannot find \"show\" function for:" ETHEN
		ERRTEXT   "\n*** Expression : "   ETHEN ERREXPR(inputExpr);
		ERRTEXT   "\n*** Of type    : "   ETHEN ERRTYPE(type);
		ERRTEXT   "\n"
		EEND;
	    }
	    printer = ap(nameShowsPrec,d);
	}
	inputExpr = ap(ap(ap(printer,mkInt(MIN_PREC)),inputExpr),nameNil);
	inputExpr = ap(namePutStr,inputExpr);
    }
    inputExpr = ap(nameIORun,inputExpr);
    compileExp();                       
    clearStack();
    run(inputCode,sp);  /* Build graph for redex */
#if DEBUG_CODE
    if (debugCode) {
	Printf("evaluator() builds: ");
	printExp(stdout,top());
	Putchar('\n');
    }
#endif
    numCells      = 0;
    numReductions = 0;
    numGcs        = 0;
    printing      = TRUE;
#if OBSERVATIONS
    appNum        = 0;
    obsCount      = 0;
    clearAllBreak();
    clearObserve();
#endif
    consGC = FALSE;
    if (nonNull(type) && addType) {
	onto(NIL);
	pushed(0) = pushed(1);
	pushed(1) = type;
	if (nonNull(temp = evalWithNoError(pop()))) {
	    abandon("Program execution",temp);
	}
	drop();
	if (whnfHead == nameRight) {
#if HUGS_FOR_WINDOWS
	    INT svColor = SetForeColor(BLUE);
#endif
	    Printf(" :: ");
	    printType(stdout,pop());
#if HUGS_FOR_WINDOWS
	    SetForeColor(svColor);
#endif
	}
    }
    else {
	if (nonNull(temp = evalWithNoError(pop()))) {
	    abandon("Program execution",temp);
	}
    }
    stopAnyPrinting();
}

/* --------------------------------------------------------------------------
 * Read in prelude module(s):
 * ------------------------------------------------------------------------*/
Void loadPrelude() {  /* load in the Prelude module(s). */
    String prelLocation;
    Bool   listFlg;

    if (!hugsPath)
	fatal("Hugs search path not defined");

    if (!( prelLocation = findMPathname(STD_PRELUDE_HUGS)) ) {
	Printf("%s not found on current path: \"%s\"\n",
	       STD_PRELUDE_HUGS, hugsPath);
	fatal("Unable to load prelude implementation");
    }
    addScriptName(prelLocation, FALSE);
    
    /* add the H98 Prelude module to the stack */
    if (!( prelLocation = findMPathname(STD_PRELUDE)) ) {
	Printf("%s not found on current path: \"%s\"\n",
	       STD_PRELUDE, hugsPath);
	fatal("Unable to load prelude");
    }
    addScriptName(prelLocation, FALSE);

    everybody(INSTALL);

    /* Hack to temporarily turn off 'listScripts' feature. */
    listFlg = listScripts;
    listScripts = FALSE;
    readScripts(0);
    listScripts = listFlg;
}

/* --------------------------------------------------------------------------
 * Send message to each component of system:
 * ------------------------------------------------------------------------*/

Void everybody(what)            /* send command `what' to each component of*/
Int what; {                     /* system to respond as appropriate ...    */
    machdep(what);              /* The order of calling each component is  */
    storage(what);              /* important for the INSTALL command       */
    substitution(what);
    input(what);
    staticAnalysis(what);
    typeChecker(what);
    compiler(what);
    machine(what);
    charOps(what);
    builtIn(what);
    controlFuns(what);
    plugins(what);
    ffi(what);
    script(what);
}
