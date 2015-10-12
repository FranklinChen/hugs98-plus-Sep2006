/* --------------------------------------------------------------------------
 * Command interpreter
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2005, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: hugs.c,v $
 * $Revision: 1.150 $
 * $Date: 2006/08/05 15:49:59 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "command.h"
#include "connect.h"
#include "errors.h"
#include "script.h"
#include "opts.h"
#include "strutil.h"
#include "evaluator.h"
#include "machdep.h"
#include "output.h"
#include "module.h"
#include <setjmp.h>
#include <ctype.h>

#include <stdio.h>

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void   local interpreter       Args((Int,String []));
static Void   local initInterpreter   Args((Void));
static Void   local menu              Args((Void));
static Void   local guidance          Args((Void));
static Void   local forHelp           Args((Void));
static Void   local changeDir         Args((Void));
static Void   local load              Args((Void));
static Void   local editor            Args((Void));
static Void   local find              Args((Void));
static Void   local setModule         Args((Void));
static Void   local showtype          Args((Void));
static String local objToStr          Args((Module, Cell));
static Void   local info              Args((Void));
static Void   local printSyntax       Args((Name));
static Void   local showInst          Args((Inst));
static Void   local describe          Args((Text));
static Void   local listNames         Args((Void));
static Void   local expandPath        Args((String,String,unsigned int));
static Void   local browse	      Args((Void));
static Void   local initialize        Args((Int, String []));
static Void   local clearEvalModule   Args((Void));

#if HUGS_FOR_WINDOWS
static Void   local autoReloadFiles   Args((Void));
#endif

/* --------------------------------------------------------------------------
 * Optional timer hooks:
 * ------------------------------------------------------------------------*/
#if WANT_TIMER
#include "timer.c"
#endif

/* --------------------------------------------------------------------------
 * Local data areas:
 * ------------------------------------------------------------------------*/
static Text    evalModule = 0;  /* Name of module we eval exprs in */
static String  defaultArgv[] = { "Hugs" };  /* program name */

/* --------------------------------------------------------------------------
 * UI interpreter initalization:
 * ------------------------------------------------------------------------*/
static Void local initialize(argc,argv)
Int    argc;
String argv[]; {
    startEvaluator();

    setLastEdit((String)0,0);

#if HUGS_FOR_WINDOWS || HAVE_WINDOWS_H
#define DEFAULT_EDITOR "\\notepad.exe"
    /*
     * Check first to see if the user has explicitly defined
     * an editor via the environment variable EDITOR..
     */
    hugsEdit      = strCopy(fromEnv("EDITOR",NULL));
    if (hugsEdit == NULL) {
#if HUGS_FOR_WINDOWS
      hugsEdit = WinHugsPickDefaultEditor();
#else
      UINT rc;
      int notePadLen = strlen(DEFAULT_EDITOR);
      char* notePadLoc;
      /*
       * Nope, the default editor is used instead. In our case
       * this is 'notepad', which we assume is always residing
       * in the windows directory, so locate it first..
       * (it would be somewhat odd for a user not to have that
       * directory in his/her PATH, but the less we assume, the better.)
       */
      notePadLoc = 
#if HAVE_ALLOCA
	  alloca
#else
          _alloca
#endif
		  (sizeof(char)*(MAX_PATH + notePadLen + 2));
      notePadLoc[0] = '&';
      rc = GetWindowsDirectory(notePadLoc+1, MAX_PATH);
      if ( !(rc == 0 || rc > MAX_PATH) ) {
	strcat(notePadLoc, DEFAULT_EDITOR);
	hugsEdit = strCopy(notePadLoc);
      }
#endif
    }
#elif __MWERKS__ && macintosh
    hugsEdit      = NULL;
#else
    hugsEdit      = strCopy(fromEnv("EDITOR",NULL));
#endif

    readOptions("-p\"%s> \" -r$$",FALSE);
    readOptionSettings();
    processOptionVector(argc,argv);

#if !HASKELL_98_ONLY
    if (haskell98) {
	Printf("Haskell 98 mode: Restart with command line option -98 to enable extensions\n\n");
    } else {
	Printf("Hugs mode: Restart with command line option +98 for Haskell 98 mode\n\n");
    }
#endif

    /* Figure out what Prelude module we're using + hoist it in. */
    loadPrelude();

    /* Add an empty module as the default, to avoid being inside the Prelude */
    addScriptName(STD_EMPTY_MODULE, TRUE);

    /* We record the number of scripts that loading the Prelude
     * brought about, so that when the user comes to clear the module
     * stack (e.g., ":l<ENTER>"), only modules later than the Prelude
     * ones are scratched.
     */
    setScriptStableMark();
    
    addScriptsFromArgs(argc,argv);
    setHugsArgs(1, defaultArgv);

    clearEvalModule();		/* evaluate wrt last module by default */
    readScripts(0);
}


/* --------------------------------------------------------------------------
 * Printing the banner
 * ------------------------------------------------------------------------*/
static Void printBanner Args((Void));

static Void printBanner()
{
#if SMALL_BANNER
    Printf("Hugs98 - http://haskell.org/hugs - %s\n", versionString);
#elif HUGS_FOR_WINDOWS
    INT svColor;
    svColor = SetForeColor(BLUE);    Printf( "__   __ __  __  ____   ___");
                                     Printf("      _______________________________________________\n");
    SetForeColor(svColor);
    svColor = SetForeColor(RED);     Printf("||   || ||  || ||  || ||__ ");
    SetForeColor(svColor);           Printf("     Hugs 98: Based on the Haskell 98 standard\n");
    svColor = SetForeColor(BLUE);    Printf("||___|| ||__|| ||__||  __||");
    SetForeColor(svColor);           Printf("     Copyright (c) 1994-2005\n");
    svColor = SetForeColor(RED);     Printf("||---||         ___||      ");
    SetForeColor(svColor);           Printf("     World Wide Web: ");
    WinHugsHyperlink("http://haskell.org/hugs");
    Printf("\n");
    svColor = SetForeColor(BLUE);    Printf("||   ||                    ");
    SetForeColor(svColor);           Printf("     Bugs: ");
    WinHugsHyperlink("http://hackage.haskell.org/trac/hugs");
    Printf("\n");
    svColor = SetForeColor(RED);     Printf("||   || ");
    SetForeColor(svColor);           Printf("Version: %-14s",versionString);
    svColor = SetForeColor(BLUE);    Printf(" _______________________________________________\n\n");
    SetForeColor(svColor);
#else
    Printf("__   __ __  __  ____   ___      _________________________________________\n");
    Printf("||   || ||  || ||  || ||__      Hugs 98: Based on the Haskell 98 standard\n");
    Printf("||___|| ||__|| ||__||  __||     Copyright (c) 1994-2005\n");
    Printf("||---||         ___||           World Wide Web: http://haskell.org/hugs\n");
    Printf("||   ||                         Bugs: http://hackage.haskell.org/trac/hugs\n");
    Printf("||   || Version: %-14s _________________________________________\n\n",versionString);
#endif

    FlushStdout();
}

/* --------------------------------------------------------------------------
 * Hugs entry point:
 * ------------------------------------------------------------------------*/

int main Args((Int, String []));       /* now every func has a prototype  */

int main(argc,argv)
int  argc;
char *argv[]; {

    CStackBase = &argc;                 /* Save stack base for use in gc   */

    if (!initSystem()) {
      Printf("%0: failed to initialize, exiting\n", (argv ? argv[0] : ""));
      return 1;
    }

    printBanner();

    interpreter(argc,argv);
    Printf("[Leaving Hugs]\n");
    everybody(EXIT);
    shutdownHugs();

    return 0;
}

/* --------------------------------------------------------------------------
 * Shutdown interpreter.
 * ------------------------------------------------------------------------*/
Void shutdownHugs() {
  /* Let go of dynamic storage */  
  if (hugsEdit)  { free(hugsEdit);  hugsEdit=0; }
  /* empties lastEdit state (and frees it up.) */
  setLastEdit((String)0,0);
  stopEvaluator();
}

/* --------------------------------------------------------------------------
 * Print Menu of list of commands:
 * ------------------------------------------------------------------------*/

static struct cmd cmds[] = {
 {":?",      HELP},   {":cd",   CHGDIR},  {":also",    ALSO},
 {":type",   TYPEOF}, {":!",    SYSTEM},  {":load",    LOAD},
 {":reload", RELOAD}, {":gc",   COLLECT}, {":edit",    EDIT},
 {":quit",   QUIT},   {":set",  SET},     {":find",    FIND},
 {":names",  NAMES},  {":info", INFO},    {":module",  SETMODULE}, 
 {":browse", BROWSE}, {":main", MAIN},
#if EXPLAIN_INSTANCE_RESOLUTION
 {":xplain", XPLAIN},
#endif
 {":version", PNTVER},
#ifdef __SYMBIAN32__
 {":Pwd",PRNDIR},
#endif
 {"",      EVAL},
 {0,0}
};

static Void local menu() {
    Printf("LIST OF COMMANDS:  Any command may be abbreviated to :c where\n");
    Printf("c is the first character in the full name.\n\n");
    Printf(":load <filenames>   load modules from specified files\n");
    Printf(":load               clear all files except prelude\n");
    Printf(":also <filenames>   read additional modules\n");
    Printf(":reload             repeat last load command\n");
    Printf(":edit <filename>    edit file\n");
    Printf(":edit               edit last module\n");
    Printf(":module <module>    set module for evaluating expressions\n");
    Printf("<expr>              evaluate expression\n");
    Printf(":type <expr>        print type of expression\n");
    Printf(":?                  display this list of commands\n");
    Printf(":set <options>      set command line options\n");
    Printf(":set                help on command line options\n");
    Printf(":names [pat]        list names currently in scope\n");
    Printf(":info <names>       describe named objects\n");
    Printf(":browse <modules>   browse names exported by <modules>\n");
    Printf(":main <aruments>    run the main function with the given arguments\n");
#if EXPLAIN_INSTANCE_RESOLUTION
    Printf(":xplain <context>   explain instance resolution for <context>\n");
#endif
    Printf(":find <name>        edit module containing definition of name\n");
#if 0
    Printf(":!command           shell escape\n");
#endif
    Printf(":cd dir             change directory\n");
    Printf(":gc                 force garbage collection\n");
#ifdef __SYMBIAN32__
    Printf(":Pwd                print working directory\n");
#endif
    Printf(":version            print Hugs version\n");
    Printf(":quit               exit Hugs interpreter\n");
}

static Void local guidance() {
    Printf("Command not recognised.  ");
    forHelp();
}

static Void local forHelp() {
    Printf("Type :? for help\n");
}

/* --------------------------------------------------------------------------
 * Change directory command:
 * ------------------------------------------------------------------------*/

/*
 * Poor man's path expansion: expand out ~/ 
 */
static Void local expandPath(origPath,expandedPath,maxLen)
String origPath;
String expandedPath;
unsigned int maxLen;
{

  if (!origPath) {
    return;
  }

  /* If the original path starts with "~/", expand it. */
  if (*origPath == '~' && *(origPath+1) == '/') {
    unsigned int origLen;
    String home          = getenv("HOME");
    origLen = (origPath ? strlen(origPath) : 0);
    /* The expansion of $HOME will fit in iff
     *    (maxLength - length(unexpanded) - length("~")) >= length("$HOME")
     */
    if ( (maxLen - origLen - 1) >= strlen(home) ) {
      strcpy(expandedPath, home);
      strcat(expandedPath, origPath+1);
      return;
    }
  }
  strcpy(expandedPath, origPath);
}

static Void local changeDir() {         /* change directory                */
    String path = readFilename();
    char expandedPath[FILENAME_MAX+1];
    expandPath(path, expandedPath,FILENAME_MAX);
    if (path && chdir(expandedPath)) {
	ERRMSG(0) "Unable to change to directory \"%s\"", path
	EEND;
    }
}

#ifdef __SYMBIAN32__
/* --------------------------------------------------------------------------
 * Print working directory command:
 * ------------------------------------------------------------------------*/

static Void local printDir() {         /* print directory                */
    char s[256];
    printf("%s\n",getcwd(s,255));
}
#endif

/* --------------------------------------------------------------------------
 * Commands for loading and removing script files:
 * ------------------------------------------------------------------------*/

static Void local load() {           /* read filenames from command line   */
    String s;                        /* and add to list of scripts waiting */
				     /* to be read                         */
    clearEvalModule();
    while ((s=readFilename())!=0) {
#if HUGS_FOR_WINDOWS
	WinHugsAddMruFile(s);
#endif
	addScriptName(s,TRUE);
    }
    readScripts(1);
}

/* --------------------------------------------------------------------------
 * Access to external editor:
 * ------------------------------------------------------------------------*/

static Void local editor() {            /* interpreter-editor interface    */
    String newFile  = readFilename();
    if (newFile) {
	setLastEdit(newFile,0);
	if (readFilename()) {
	    ERRMSG(0) "Multiple filenames not permitted"
	    EEND;
	}
    }
    runEditor();
}

static Void local find() {              /* edit file containing definition */
    String nm = readFilename();         /* of specified name               */
    if (!nm) {
	ERRMSG(0) "No name specified"
	EEND;
    }
    else if (readFilename()) {
	ERRMSG(0) "Multiple names not permitted"
	EEND;
    }
    else {
	Text t;
	Cell c;
	setCurrModule(findEvalModule());
	startNewScript(0);
	if (nonNull(c=findTycon(t=findText(nm)))) {
	    if ( startEdit(tycon(c).line,getScriptName(scriptThisTycon(c))) ) {
		readScripts(1);
	    }
	} else if (nonNull(c=findName(t))) {
	    if ( startEdit(name(c).line,getScriptName(scriptThisName(c))) ) {
		readScripts(1);
	    }
	} else {
	    ERRMSG(0) "No current definition for name \"%s\"", nm
	    EEND;
	}
    }
}

Void runEditor() {         /* run editor on script lastEdit   */
    String fileToEdit;
    Int    lastLine;
    String lastEdit = getLastEdit(&lastLine);

    if (lastEdit == NULL) {
      fileToEdit = fileOfModule(lastModule());
    } else {
      fileToEdit = lastEdit;
    }
    if (startEdit(lastLine,fileToEdit)) { /* at line lastLine              */
        /* reload entire module stack bar the Prelude. */
	readScripts(1);
    }
}

/* --------------------------------------------------------------------------
 * Read and evaluate an expression:
 * ------------------------------------------------------------------------*/

static Void local setModule(){/*set module in which to evaluate expressions*/
    String s = readFilename();
    if (s!=0) {			/* Locate named module			   */
	Text    t = findText(s);
	Module  m = findModule(t);
	if (isNull(m)) {
	    ERRMSG(0) "Cannot find module \"%s\"", s
	    EEND;
	}
	else {
	    evalModule = t;
	    setLastEdit(fileOfModule(m),0);
	}
    }
    else {			/* :m clears the current module selection */
	clearEvalModule();
	setLastEdit(fileOfModule(lastModule()),0);
    }
}

Module findEvalModule() { /*Module in which to eval expressions*/
    Module m = findModule(evalModule); 
    if (isNull(m))
	m = lastModule();
    return m;
}

static Void local clearEvalModule() {
    evalModule = findText("");
}

/* --------------------------------------------------------------------------
 * Print type of input expression:
 * ------------------------------------------------------------------------*/

static Void local showtype() {         /* print type of expression (if any)*/
    Cell type;

    setCurrModule(findEvalModule());
    startNewScript(0);                 /* Enables recovery of storage      */
				       /* allocated during evaluation      */
    parseExp();
    checkExp();
    defaultDefns = evalDefaults;
    type = typeCheckExp(printTypeUseDefaults);
    printExp(stdout,inputExpr);
#if HUGS_FOR_WINDOWS
    { INT svColor = SetForeColor(BLUE);
#endif
    Printf(" :: ");
    printType(stdout,type);
#if HUGS_FOR_WINDOWS
    SetForeColor(svColor); }
#endif
    Putchar('\n');
}

static Void local browse() {            /* browse modules                  */
    Int    count = 0;                   /* or current module               */
    String s;
    Bool all = FALSE;

    setCurrModule(findEvalModule());
    startNewScript(0);                  /* for recovery of storage         */
    while ((s=readFilename())!=0)
	if (strcmp(s,"all") == 0) {
	    all = TRUE;
	} else {
	    Module mod = findModule(findText(s));
	    if (isNull(mod)) {
		Printf("Unknown module %s\n",s);
	    } else {
		browseModule(mod,all);
	    }
	    count++;
	}
    if (count == 0)
	browseModule(findEvalModule(),all);
}

#if EXPLAIN_INSTANCE_RESOLUTION
static Void local xplain() {         /* print type of expression (if any)*/
    Cell d;
    Bool sir = showInstRes;

    setCurrModule(findEvalModule());
    startNewScript(0);                 /* Enables recovery of storage      */
				       /* allocated during evaluation      */
    parseContext();
    checkContext();
    showInstRes = TRUE;
    d = provePred(NIL,NIL,hd(inputContext));
    if (isNull(d)) {
	fprintf(stdout, "not Sat\n");
    } else {
	fprintf(stdout, "Sat\n");
    }
    fflush(stdout);
    showInstRes = sir;
}
#endif

static Void local runmain() {
    int MaxArgs = 255;
    String args[256];
    String s;
    int argPos = 1, i;
    args[0] = "Hugs";

    while (argPos < MaxArgs && (s = readFilename())) {
	args[argPos++] = strCopy(s);
    }

    setHugsArgs(argPos, args);
    for (i = 1; i < argPos; i++)
	free(args[i]);

    stringInput((String)"main");
    input(BREAK);
    doCommand();
}

/* --------------------------------------------------------------------------
 * Enhanced help system:  print current list of scripts or give information
 * about an object.
 * ------------------------------------------------------------------------*/

static String local objToStr(m,c)
Module m;
Cell   c; {
#if 1 || DISPLAY_QUANTIFIERS
    static char newVar[60];
    switch (whatIs(c)) {
	case NAME  : if (m == name(c).mod) {
			 sprintf(newVar,"%s", textToStr(name(c).text));
		     } else {
			 sprintf(newVar,"%s.%s",
					textToStr(module(name(c).mod).text),
					textToStr(name(c).text));
		     }
		     break;

	case TYCON : if (m == tycon(c).mod) {
			 sprintf(newVar,"%s", textToStr(tycon(c).text));
		     } else {
			 sprintf(newVar,"%s.%s",
					textToStr(module(tycon(c).mod).text),
					textToStr(tycon(c).text));
		     }
		     break;

	case CLASS : if (m == cclass(c).mod) {
			 sprintf(newVar,"%s", textToStr(cclass(c).text));
		     } else {
			 sprintf(newVar,"%s.%s",
					textToStr(module(cclass(c).mod).text),
					textToStr(cclass(c).text));
		     }
		     break;

	default    : internal("objToStr");
    }
    return newVar;
#else
    static char newVar[33];
    switch (whatIs(c)) {
	case NAME  : sprintf(newVar,"%s", textToStr(name(c).text));
		     break;

	case TYCON : sprintf(newVar,"%s", textToStr(tycon(c).text));
		     break;

	case CLASS : sprintf(newVar,"%s", textToStr(cclass(c).text));
		     break;

	default    : internal("objToStr");
    }
    return newVar;
#endif
}

static Void local info() {              /* describe objects                */
    Int    count = 0;                   /* or give menu of commands        */
    String s;
    Module evMod;
    
    evMod = findEvalModule();

    setCurrModule(evMod);
    startNewScript(0);                  /* for recovery of storage         */
    for (; (s=readFilename())!=0; count++) {
        String mod=NULL;
	String nm=NULL;
	 
	/* In the event of a qualified name, decompose it. */
	splitQualString(s, &mod, &nm);

	if (mod == NULL) {
	    describe(findText(nm));
	} else {
	    Module homeMod = findModule(findText(mod));
	    if (nonNull(homeMod)) {
		setCurrModule(homeMod);
		describe(findText(nm));
	    } else
		Printf("Unknown module `%s'\n",mod);
		/* With the module unknown, don't check the name. */
	    free(mod); mod = NULL;
	}
    }
    if (count == 0) {
	whatScripts();
    }
    setCurrModule(evMod);
}

static Void local describe(t)           /* describe an object              */
Text t; {
    Tycon  tc  = findTycon(t);
    Class  cl  = findClass(t);
    Name   nm  = findName(t);

    if (nonNull(tc)) {                  /* as a type constructor           */
	Type t = tc;
	Int  i;
	Inst in;
	for (i=0; i<tycon(tc).arity; ++i) {
	    t = ap(t,mkOffset(i));
	}
	Printf("-- type constructor");
	if (kindExpert) {
	    Printf(" with kind ");
	    printKind(stdout,tycon(tc).kind);
	}
	Putchar('\n');
	switch (tycon(tc).what) {
	    case SYNONYM      : Printf("type ");
				printType(stdout,t);
				Printf(" = ");
				printType(stdout,tycon(tc).defn);
				break;

	    case NEWTYPE      :
	    case DATATYPE     : {   List cs = tycon(tc).defn;
				    if (tycon(tc).what==DATATYPE) {
					Printf("data ");
				    } else {
					Printf("newtype ");
				    }
				    printType(stdout,t);
				    Putchar('\n');
				    mapProc(printSyntax,cs);
				    if (hasCfun(cs)) {
					Printf("\n-- constructors:");
				    }
				    for (; hasCfun(cs); cs=tl(cs)) {
					Putchar('\n');
					printExp(stdout,hd(cs));
					Printf(" :: ");
					printType(stdout,name(hd(cs)).type);
				    }
				    if (nonNull(cs)) {
					Printf("\n-- selectors:");
				    }
				    for (; nonNull(cs); cs=tl(cs)) {
					Putchar('\n');
					printExp(stdout,hd(cs));
					Printf(" :: ");
					printType(stdout,name(hd(cs)).type);
				    }
				}
				break;

	    case RESTRICTSYN  : Printf("type ");
				printType(stdout,t);
				Printf(" = <restricted>");
				break;
	}
	Putchar('\n');
	if (nonNull(in=findFirstInst(tc))) {
	    Printf("\n-- instances:\n");
	    do {
		showInst(in);
		in = findNextInst(tc,in);
	    } while (nonNull(in));
	}
	Putchar('\n');
    }

    if (nonNull(cl)) {                  /* as a class                      */
	List  ins = cclass(cl).instances;
	Kinds ks  = cclass(cl).kinds;
	if (nonNull(ks) && isNull(tl(ks)) && hd(ks)==STAR) {
	    Printf("-- type class");
	} else {
	    Printf("-- constructor class");
	    if (kindExpert) {
		Printf(" with arity ");
		printKinds(stdout,ks);
	    }
	}
	Putchar('\n');
	mapProc(printSyntax,cclass(cl).members);
	Printf("class ");
	if (nonNull(cclass(cl).supers)) {
	    printContext(stdout,cclass(cl).supers);
	    Printf(" => ");
	}
	printPred(stdout,cclass(cl).head);
	if (nonNull(cclass(cl).fds)) {
	    List   fds = cclass(cl).fds;
	    String pre = " | ";
	    for (; nonNull(fds); fds=tl(fds)) {
		Printf(pre);
		printFD(stdout,hd(fds));
		pre = ", ";
	    }
	}
	if (nonNull(cclass(cl).members)) {
	    List ms = cclass(cl).members;
	    Printf(" where");
	    do {
		Type t = name(hd(ms)).type;
                if (isPolyType(t)) {
		    t = monotypeOf(t);
		}
		Printf("\n  ");
		printExp(stdout,hd(ms));
		Printf(" :: ");
		if (isNull(tl(fst(snd(t))))) {
		    t = snd(snd(t));
		} else {
		    t = ap(QUAL,pair(tl(fst(snd(t))),snd(snd(t))));
		}
		printType(stdout,t);
		ms = tl(ms);
	    } while (nonNull(ms));
	}
	Putchar('\n');
	if (nonNull(ins)) {
	    Printf("\n-- instances:\n");
	    do {
		showInst(hd(ins));
		ins = tl(ins);
	    } while (nonNull(ins));
	}
	Putchar('\n');
    }

    if (nonNull(nm)) {                  /* as a function/name              */
	printSyntax(nm);
	printExp(stdout,nm);
	Printf(" :: ");
	if (nonNull(name(nm).type)) {
	    printType(stdout,name(nm).type);
	} else {
	    Printf("<unknown type>");
	}

	if (isCfun(nm)) {
	    Printf("  -- data constructor");
	} else if (isMfun(nm)) {
	    Printf("  -- class member");
	} else if (isSfun(nm)) {
	    Printf("  -- selector function");
	}
	if (name(nm).primDef) {
	    Printf("   -- primitive");
	}
	Printf("\n\n");
    }

    if (isNull(tc) && isNull(cl) && isNull(nm)) {
	Printf("Unknown reference `%s'\n",textToStr(t));
    }
}

static Void local printSyntax(nm)
Name nm; {
    Syntax sy = syntaxOf(nm);
    Text   t  = name(nm).text;
    String s  = textToStr(t);
    if (sy != defaultSyntax(t)) {
	Printf("infix");
	switch (assocOf(sy)) {
	    case LEFT_ASS  : Putchar('l'); break;
	    case RIGHT_ASS : Putchar('r'); break;
	    case NON_ASS   : break;
	}
	Printf(" %i ",precOf(sy));
	if (isascii(*s) && isalpha(*s)) {
	    Printf("`%s`",s);
	} else {
	    Printf("%s",s);
	}
	Putchar('\n');
    }
}

static Void local showInst(in)          /* Display instance decl header    */
Inst in; {
    Printf("instance ");
    if (nonNull(inst(in).specifics)) {
	printContext(stdout,inst(in).specifics);
	Printf(" => ");
    }
    printPred(stdout,inst(in).head);
    Putchar('\n');
}

/* --------------------------------------------------------------------------
 * List all names currently in scope:
 * ------------------------------------------------------------------------*/

static Void local listNames() {         /* list names matching optional pat*/
    String pat   = readFilename();
    List   names = NIL;
    Int    width = getTerminalWidth() - 1;
    Int    count = 0;
    Int    termPos;
    Module mod   = findEvalModule();

    if (pat) {                          /* First gather names to list      */
	do {
	    names = addNamesMatching(pat,names);
	} while ((pat=readFilename())!=0);
    } else {
	names = addNamesMatching((String)0,names);
    }
    if (isNull(names)) {                /* Then print them out             */
	ERRMSG(0) "No names selected"
	EEND;
    }
    for (termPos=0; nonNull(names); names=tl(names)) {
	String s = objToStr(mod,hd(names));
	Int    l = strlen(s);
	if (termPos+1+l>width) { 
	    Putchar('\n');       
	    termPos = 0;         
	} else if (termPos>0) {  
	    Putchar(' ');        
	    termPos++;           
	}
	Printf("%s",s);
	termPos += l;
	count++;
    }
    Printf("\n(%d names listed)\n", count);
}

/* --------------------------------------------------------------------------
 * print a prompt and read a line of input:
 * ------------------------------------------------------------------------*/

/* Size of (expanded) prompt buffer, should be more than enough.... */
#define MAX_PROMPT_SIZE 1000

Void promptForInput(moduleName)
String moduleName; {
    char promptBuffer[MAX_PROMPT_SIZE];
    char* fromPtr;
    char* toPtr;
    int modLen = strlen(moduleName);
    int roomLeft = MAX_PROMPT_SIZE - 1;
    
    toPtr = promptBuffer;
    fromPtr = prompt;
    
    /* Carefully substituting occurrences of %s in the
       prompt string with the module name.
    */
    while (*fromPtr != '\0' && roomLeft > 0) {
      if (*fromPtr == '%' && *(fromPtr+1) == 's') {
	/* Substitute module name */
        if (modLen > roomLeft) {
	  /* Running out of room; copy what we can */
	  fromPtr = moduleName;
	  while (roomLeft-- > 0) {
	    *toPtr++ = *fromPtr++;
	  }
	  break;
	} else {
	  strcpy(toPtr,moduleName);
	  toPtr += modLen;
	  roomLeft -= modLen;
	  fromPtr +=2;
	}
      } else {
	*toPtr++ = *fromPtr++;
	roomLeft--;
      }
    }
    *toPtr = '\0';

    consoleInput(promptBuffer);
}

#if HUGS_FOR_WINDOWS
static Void local autoReloadFiles() {
    if (autoLoadFiles) {
      InAutoReloadFiles = TRUE;
      saveInputState();
      readScripts(1);
      restoreInputState();
      InAutoReloadFiles = FALSE;
    }
}
#endif

/* --------------------------------------------------------------------------
 * main read-eval-print loop, with error trapping:
 * ------------------------------------------------------------------------*/

static Void local interpreter(argc,argv)/* main interpreter loop           */
Int    argc;
String argv[]; {
    Int errorNumber = setjmp(catch_error);

    breakOn(TRUE);                      /* enable break trapping           */
    if ( numLoadedScripts()==0 ) {      /* only succeeds on first time,    */
	if (errorNumber)                /* before Prelude has been loaded  */
	    fatal("Unable to load Prelude");
	initialize(argc,argv);
	forHelp();
    }

#if defined(_MSC_VER) && !defined(_MANAGED)
    /* Under Win32 (when compiled with MSVC), we specially
     * catch and handle SEH stack overflows.
     */
    __try {
#endif

#ifdef HUGS_FOR_WINDOWS
    initInterpreter();
    InAutoReloadFiles = FALSE;
    WinHugsMessagePump();
#else
    for (;;) {
	initInterpreter();
	if (doCommand())
	    break;
    }
#endif

    breakOn(FALSE);
    
#if defined(_MSC_VER) && !defined(_MANAGED)
    } __except ( ((GetExceptionCode() == EXCEPTION_STACK_OVERFLOW) ?
		  EXCEPTION_EXECUTE_HANDLER :
		  EXCEPTION_CONTINUE_SEARCH) ) {
	/* Closely based on sample code in Nov 1999 Dr GUI MSDN column */
	/* http://msdn.microsoft.com/archive/en-us/dnaraskdr/html/drgui49.asp */
	char* stackPtr;
	static SYSTEM_INFO si;
	static MEMORY_BASIC_INFORMATION mi;
	static DWORD protect;

	/* get at the current stack pointer */
	_asm mov stackPtr, esp;

	/* query for page size + VM info for the allocation chunk
	   we're currently in. */
	GetSystemInfo(&si);
	VirtualQuery(stackPtr, &mi, sizeof(mi));

	/* Abandon the C stack and, most importantly, re-insert
	   the page guard bit. Do this on the page above the
	   current one, not the one where the exception was raised. */
	stackPtr = (LPBYTE) (mi.BaseAddress) - si.dwPageSize;
	if ( VirtualFree(mi.AllocationBase,
			 (LPBYTE)stackPtr - (LPBYTE) mi.AllocationBase,
			 MEM_DECOMMIT) &&
	     VirtualProtect(stackPtr, si.dwPageSize,
			    PAGE_GUARD | PAGE_READWRITE, &protect) ) {

	    /* careful not to do a garbage collection here
	       (as it may have caused the overflow). */
	    ERRTEXT "ERROR - C stack overflow"
	    /* EEND does a longjmp back to a sane state. */
	    EEND;
	} else {
	    fatal("C stack overflow; unable to recover.");
	}
    }
#endif
}

static Void local initInterpreter()
{
    everybody(RESET);               /* reset to sensible initial state */
    dropScriptsFrom(numLoadedScripts()-1); 
				    /* remove partially loaded scripts */
				    /* not counting prelude as a script*/

    promptForInput(textToStr(module(findEvalModule()).text));
}

Bool doCommand()		    /* read and execute a command      */
{				    /* returns TRUE on QUIT (:quit)    */
	Command cmd;
	cmd = readCommand(cmds, (Char)':', (Char)'!');
#if WANT_TIMER
	updateTimers();
#endif
	switch (cmd) {
	    case EDIT   : editor();
			  break;
	    case FIND   : 
#if HUGS_FOR_WINDOWS
			  autoReloadFiles();
#endif
                          find();
			  break;
	    case LOAD   : forgetAllScripts();
			  load();
			  break;
	    case ALSO   : forgetScriptsFrom(numLoadedScripts());
			  load();
			  break;
	    case RELOAD : readScripts(1);
			  break;
	    case SETMODULE :
			  setModule();
			  break;
	    case EVAL   : 
#if HUGS_FOR_WINDOWS
			  autoReloadFiles();
#endif
#if USE_THREADS
                          startEvaluatorThread();
			  loopInBackground();
#else
			  evaluator(findEvalModule());
#endif
			  break;
	    case TYPEOF : 
#if HUGS_FOR_WINDOWS
			  autoReloadFiles();
#endif
                          showtype();
			  break;
	    case BROWSE : browse();
			  break;
#if EXPLAIN_INSTANCE_RESOLUTION
	    case XPLAIN : xplain();
			  break;
#endif
	    case NAMES  : 
#if HUGS_FOR_WINDOWS
			  autoReloadFiles();
#endif
                          listNames();
			  break;
	    case HELP   : menu();
			  break;
	    case BADCMD : guidance();
			  break;
	    case SET    : setOptions();
			  break;
  	    case SYSTEM : if (shellEsc(readLine(),TRUE,TRUE))
			      Printf("Warning: Shell escape terminated abnormally\n");
			  break;
	    case CHGDIR : changeDir();
			  break;
	    case INFO   : 
#if HUGS_FOR_WINDOWS
			  autoReloadFiles();
#endif
                          info();
			  break;
	    case PNTVER: Printf("-- Hugs Version %s\n", versionString);
			  break;
	    case QUIT   : return TRUE;
	    case COLLECT: consGC = FALSE;
			  garbageCollect();
			  consGC = TRUE;
			  Printf("Garbage collection recovered %d cells\n",
				 cellsRecovered);
			  break;
	    case NOCMD  : break;
	    case MAIN: runmain();
	    	  break;
#ifdef __SYMBIAN32__
        case PRNDIR : printDir();
              break;
#endif
	}
#if WANT_TIMER
	updateTimers();
	Printf("Elapsed time (ms): %ld (user), %ld (system)\n",
	       millisecs(userElapsed), millisecs(systElapsed));
#endif
	return FALSE;
}

/*-------------------------------------------------------------------------*/

