/*
 * Maintaining a stack of files / scripts.
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
#include "machdep.h"
#include "opts.h"
#include "strutil.h"
#include "script.h"

/* --------------------------------------------------------------------------
 * Local script state:
 * ------------------------------------------------------------------------*/

/*
 * The scripts that either have been loaded or will be later are all
 * stored in fixed-size stacks. (Moving to growable tables will also 
 * require making the module table expandable.)
 */
struct strScript {
    String fileName;			/* Script file name                */
    String realName;			/* Full path to canonical name     */
    String directory;			/* Directory module was found in   */
    Time lastChange;			/* Time of last change to script   */
    Bool postponed;			/* Indicates postponed load        */
    Bool chased;			/* Added by import chasing?        */
};

static struct strScript scriptTable[NUM_SCRIPTS];

static Int    numScripts;               /* Number of scripts loaded        */
static Int    namesUpto;                /* Number of script names set      */
static Int    scriptsStable;            /* Number of (Prelude) scripts     */
                                        /* considered 'stable'             */
                                        /* (=> won't be nuked when clearing */
                                        /* the script stack / reloading.)   */

static Bool   needsImports;             /* set to TRUE if imports required */
       String scriptFile;               /* Name of current script (if any) */

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/
static Bool local addScript     Args((String,Long));
static Void local freeScript    Args((Int));

/* --------------------------------------------------------------------------
 * Initialising / freeing script stacks:
 * ------------------------------------------------------------------------*/
Void initScripts() {
  scriptFile    = 0;
  numScripts    = 0;
  namesUpto     = 0;
  scriptsStable = 0;
}

Void stopScripts() {
  int i;

  for (i=0; i < numScripts ; i++)
    freeScript(i);
}

static Void local freeScript(i)
Int i; {
  if (scriptTable[i].fileName)
    free(scriptTable[i].fileName);
  if (scriptTable[i].realName)
    free(scriptTable[i].realName);
  if (scriptTable[i].directory)
    free(scriptTable[i].directory);
}

/* We record the number of scripts that loading the Prelude
 * brought about, so that when the user comes to clear the module
 * stack (e.g., ":l<ENTER>"), only modules later than the Prelude
 * ones are scratched.
 */
Void setScriptStableMark() {
  scriptsStable = namesUpto;
}

String getScriptName(s)  /* access the script name at index 's' */
Script s; {
  if ( s >=0 && s <= numScripts ) {
    return scriptTable[s].fileName;
  } else {
    ERRMSG(0) "getScriptName: Illegal script index %d (max: %d)", s, numScripts
    EEND;
  }
  return NULL;
}

String getScriptRealName(s)  /* access the path of script at index 's' */
Script s; {
  if ( s >=0 && s <= numScripts ) {
    return scriptTable[s].realName;
  } else {
    ERRMSG(0) "getScriptRealName: Illegal script index %d (max: %d)", s, numScripts
    EEND;
  }
  return NULL;
}

Int getScriptHwMark() { /* return number of on the stack, loaded or not. */
  return namesUpto;
}

Int numLoadedScripts() { /* return number of currently loaded scripts */
  return numScripts;
}

#if HUGS_FOR_WINDOWS
/* UI pokes around the script stack, give it access... */
Void setNumLoadedScripts(s)
Script s; {
  numScripts=s;
}

Void setScriptHwMark(s)
Script s; {
  namesUpto = s;
}

Void setScriptName(s,scr)
Script s;
String scr; {
  scriptTable[s].fileName = scr;
}

Void setScriptRealName(s,scr)
Script s;
String scr; {
  scriptTable[s].realName = scr;
}
#endif

/* --------------------------------------------------------------------------
 * Loading script files:
 * ------------------------------------------------------------------------*/

Void addScriptName(s,sch)  /* Add script to list of scripts   */
String s;                  /* to be read in ...               */
Bool   sch; {              /* TRUE => requires pathname search*/
    if (namesUpto>=NUM_SCRIPTS) {
	ERRMSG(0) "Too many module files (maximum of %d allowed)",
		  NUM_SCRIPTS
	EEND;
	return;
    }
    if (sch) {
	if (isModuleId(s)) {
	    String location = findMPathname(s);
	    if (!location) {
		ERRMSG(0) "Can't find module \"%s\"", s
		EEND;
	    }
	    scriptTable[namesUpto].fileName  = strCopy(location);
	    scriptTable[namesUpto].directory = NULL;
	} else {
	    scriptTable[namesUpto].fileName  = strCopy(findPathname(s));
	    scriptTable[namesUpto].directory = dirname(scriptTable[namesUpto].fileName);
	}
    } else {
	scriptTable[namesUpto].fileName  = strCopy(s);
	scriptTable[namesUpto].directory = NULL;
    }
    scriptTable[namesUpto].realName   = strCopy(RealPath(scriptTable[namesUpto].fileName));
    scriptTable[namesUpto].chased     = !sch;
    namesUpto++;
}

static Bool local addScript(fname,len)  /* read single script file */
String fname;                           /* name of script file     */
Long   len; {                           /* length of script file   */
#if HUGS_FOR_WINDOWS         /* Set clock cursor while loading   */
    allowBreak();
    SetCursor(LoadCursor(NULL, IDC_WAIT));
#endif

    if (!quiet) {
	Printf("Reading file \"%s\":\n",fname);  FlushStdout();
    }
    setLastEdit(fname,0);

    needsImports = FALSE;
    scriptFile = 0;
    if (!parseScript(fname,len)) {   /* process script file */
	/* file or parse error, drop the script */ 
	forgetAScript(numScripts);
	errFail();
    }
    if (needsImports) return FALSE;
    checkDefns();
    typeCheckDefns();
    compileDefns();
    scriptFile    = 0;
    preludeLoaded = TRUE;
    return TRUE;
}

Bool chase(imps)                 /* Process list of import requests */
List imps; {
    Int    origPos = numScripts; /* keep track of original position */
    String origDir = scriptTable[origPos].directory;
    for (; nonNull(imps); imps=tl(imps)) {
	String modname = textToStr(textOf(hd(imps)));
	String iname = NULL;
	String rname;
	Bool   inOrigDir = FALSE;
	Int    i     = 0;

	if (origDir) {
	    iname = findMInDir(origDir,modname);
	    if (iname)
		inOrigDir = TRUE;
	}
	if (iname == NULL)
	    iname = findMPathname(modname);
	if (iname == NULL) {
	    ERRMSG(0) "Can't find imported module \"%s\"", modname
	    EEND;
	}

	rname = RealPath(iname);
	for (; i<namesUpto; i++)
	    if (filenamecmp(scriptTable[i].realName,rname)==0)
		break;
	if (i>=origPos) {           /* Neither loaded or queued        */
	    struct strScript tmpScript;

	    scriptTable[origPos].postponed = TRUE;
	    needsImports           = TRUE;

	    if (i>=namesUpto) {     /* Name not found (i==namesUpto)   */
		addScriptName(iname,FALSE);
		if (inOrigDir)
		    scriptTable[i].directory = strCopy(origDir);
	    } else if (scriptTable[i].postponed) {/* imported by itself? */
		ERRMSG(0)
		  "Recursive import dependency between \"%s\" and \"%s\"",
		  scriptTable[origPos].fileName, iname
		EEND;
	    }
	    /* Right rotate section of tables between numScripts and i so
	     * that i ends up with other imports in front of orig. script
	     */
	    tmpScript = scriptTable[i];
	    for (; i>numScripts; i--)
		scriptTable[i] = scriptTable[i-1];
	    scriptTable[numScripts] = tmpScript;
	    origPos++;
	}
    }
    return needsImports;
}

/* --------------------------------------------------------------------------
 * Adding scripts found in an argument vector:
 * ------------------------------------------------------------------------*/
Void addScriptsFromArgs(argc,argv)
Int argc;
String argv[]; {
    Int i;

#if USE_PREFERENCES_FILE
    extern Int     iniArgc;
    extern String* iniArgv;

    if (iniArgc > 0) {
        /* load additional files found in the preferences file */
        for (i=0; i<iniArgc; i++) {
	    addScriptName(iniArgv[i],TRUE);
        }
    }
#endif
    for (i=1; i<argc; ++i) {
      if (argv[i] && argv[i][0] && !isOption(argv[i])) {
	    addScriptName(argv[i],TRUE);
#if HUGS_FOR_WINDOWS
	    SetWorkingDir(argv[i]);
#endif
      }
    }


}

/* --------------------------------------------------------------------------
 * Dropping script files:
 * ------------------------------------------------------------------------*/
Void forgetScriptsFrom(scno) /* remove scripts from system     */
Script scno; {
    Script i;
    for (i=scno; i<namesUpto; ++i)
	freeScript(i);
    dropScriptsFrom(scno-1); /* don't count prelude as script  */
    namesUpto = scno;
    if (numScripts>namesUpto)
	numScripts = scno;
}

Void forgetAllScripts() {
  /* Drop all but the stable scripts; i.e., the
   * Prelude and (possibly) its implementation module(s).
   */
  forgetScriptsFrom( scriptsStable ); 
}

Void forgetAScript(scno) /* remove a script from system */
Script scno; {
    Script i;
    
    if (scno > namesUpto)
	return;

    freeScript(scno);

    for (i=scno+1; i < namesUpto; i++)
	scriptTable[i-1] = scriptTable[i];
    dropAScript(scno);
    namesUpto--;
}

Void readScripts(n)        /* Reread current list of scripts, */
Int n; {                   /* loading everything after and    */
    Time timeStamp;        /* including the first script which*/
    Long fileSize;         /* has been either changed or added*/

#if HUGS_FOR_WINDOWS
    SetCursor(LoadCursor(NULL, IDC_WAIT));
#endif

    for (; n<numScripts; n++) {         /* Scan previously loaded scripts  */
	getFileInfo(scriptTable[n].fileName, &timeStamp, &fileSize);
	if (timeChanged(timeStamp,scriptTable[n].lastChange)) {
	    dropScriptsFrom(n-1);
	    numScripts = n;
	    break;
	}
    }
    for (; n<NUM_SCRIPTS; n++)          /* No scripts have been postponed  */
	scriptTable[n].postponed = FALSE;       /* at this stage                   */


    while (numScripts<namesUpto) {      /* Process any remaining scripts   */
	getFileInfo(scriptTable[numScripts].fileName, &timeStamp, &fileSize);
	timeSet(scriptTable[numScripts].lastChange,timeStamp);
	if (numScripts>0)               /* no new script for prelude       */
	    startNewScript(scriptTable[numScripts].fileName);
        generate_ffi = generateFFI && !scriptTable[numScripts].chased;
	if (addScript(scriptTable[numScripts].fileName,fileSize))
	    numScripts++;
	else
	    dropScriptsFrom(numScripts-1);
    }

    if (listScripts)
	whatScripts();
    if (numScripts<=1)
	setLastEdit((String)0, 0);
}

Void whatScripts() {       /* list scripts in current session */
    int i;
#if HUGS_FOR_WINDOWS
    if (!InAutoReloadFiles) {
#endif
    Printf("\nHugs session for:");
    for (i=0; i<numScripts; ++i) {
#if HUGS_FOR_WINDOWS
	Putchar('\n');
	WinHugsFilename(scriptTable[i].fileName, 0);
#else
	Printf("\n%s",scriptTable[i].fileName);
#endif
    }
    Putchar('\n');
#if HUGS_FOR_WINDOWS
    }
#endif
}

Void script(what)
Int what; {

  /* not much to it..will be more when/if the local tables
   * become resizable.
   */
  switch(what) {
  case RESET:   return;
  case MARK:    return;
  case INSTALL: return;
  case EXIT:    stopScripts();
                return;
  case BREAK:   return;
  }
}
  
