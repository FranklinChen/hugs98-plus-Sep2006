/*
 * Processing options/toggles.
 *
 *
 */
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "machdep.h"
#include "strutil.h"
#include "opts.h"
#include "char.h"

/* --------------------------------------------------------------------------
 * Flags and options:
 * ------------------------------------------------------------------------*/
Bool   preludeLoaded	 = FALSE;

Bool   showStats    = FALSE;     /* TRUE => print stats after eval  */
Bool   addType      = FALSE;     /* TRUE => print type with value   */
Bool   useShow      = TRUE;      /* TRUE => use Text/show printer   */
Bool   displayIO    = FALSE;     /* TRUE => use printer for IO result*/
Bool   useDots      = RISCOS;    /* TRUE => use dots in progress    */
Bool   listScripts  = FALSE;     /* TRUE => list scripts after loading*/
Bool   quiet        = TRUE;      /* TRUE => don't show progress     */
Bool   printing     = FALSE;     /* TRUE => currently printing value*/
String  hugsEdit   = 0;	         /* String for editor command       */
String  prompt     = 0;          /* Prompt string                   */

#if !HASKELL_98_ONLY
Bool haskell98 = TRUE;		 /* TRUE => Haskell 98 compatibility*/
#endif

#if EXPLAIN_INSTANCE_RESOLUTION
Bool showInstRes = FALSE;
#endif
#if MULTI_INST
Bool multiInstRes = FALSE;
#endif

Bool printTypeUseDefaults = FALSE;


#if USE_PREFERENCES_FILE
/* --------------------------------------------------------------------------
 * Decls for MacOS preference file support:
 * ------------------------------------------------------------------------*/
static Void    readPrefsFile          Args((FILE *));
typedef char GVarname[2000];
static GVarname hugsFlags = "";
int  iniArgc;
char iniArgv[10][33];
#endif

/* --------------------------------------------------------------------------
 * Keeping track of current file:
 * ------------------------------------------------------------------------*/
static String  lastEdit   = 0;   /* Name of script to edit (if any) */
static Int     lastLine   = 0;   /* Editor line number (if possible)*/

Void setLastEdit(fname,line) /* keep name of last file to edit */
String fname;
Int    line; {
    if (lastEdit) {
      free(lastEdit);
    }
    lastEdit = strCopy(fname);
    lastLine = line;
}

String getLastEdit(Int* pLastLine) {
  if (pLastLine)
    *pLastLine = lastLine;
  return lastEdit;
}

/* --------------------------------------------------------------------------
 * Setting of command line options:
 * ------------------------------------------------------------------------*/

#if HASKELL_98_ONLY
#define Option(c,h98,description,flag) { c, description, flag }
#else
#define Option(c,h98,description,flag) { c, h98, description, flag }
#endif

static Void   local toggleSet         Args((Char,Bool));
static Void   local togglesIn         Args((Bool));

struct options toggle[] = {     /* List of command line toggles    */
    Option('s', 1, "Print no. reductions/cells after eval", &showStats),
    Option('t', 1, "Print type after evaluation",           &addType),
    Option('g', 1, "Print no. cells recovered after gc",    &gcMessages),
    Option('l', 1, "Literate modules as default",           &literateScripts),
    Option('.', 1, "Print dots to show progress",           &useDots),
    Option('q', 1, "Print nothing to show progress",        &quiet),
    Option('Q', 1, "Qualify names when printing",           &useQualifiedNames),
    Option('w', 1, "Always show which modules are loaded",  &listScripts),
    Option('k', 1, "Show kind errors in full",              &kindExpert),
    Option('o', 0, "Allow overlapping instances",           &allowOverlap),
    Option('O', 0, "Allow unsafe overlapping instances",    &allowUnsafeOverlap),
    Option('u', 1, "Use \"show\" to display results",       &useShow),
    Option('I', 1, "Display results of IO programs",        &displayIO),
    Option('T', 1, "Apply 'defaulting' when printing types", &printTypeUseDefaults),
/* Conditional toggles: */
#if HUGS_FOR_WINDOWS
    Option('A', 1, "Auto load files",		   	    &autoLoadFiles),
#endif
#if EXPLAIN_INSTANCE_RESOLUTION
    Option('x', 1, "Explain instance resolution",           &showInstRes),
#endif
#if MULTI_INST
    Option('m', 0, "Use multi instance resolution",         &multiInstRes),
#endif
#if DEBUG_CODE
    Option('D', 1, "Debug: show generated G code",          &debugCode),
#endif
#if DEBUG_SHOWSC
    Option('C', 1, "Debug: show generated SC code",         &debugSC),
#endif
#if OBSERVATIONS
    Option('R', 1, "Enable root optimisation",              &rootOpt),
#endif
#if HERE_DOC
    Option('H', 0, "Enable `here documents'",               &hereDocs),
#endif
    Option(0, 0, 0, 0)
};

Void setOptions(Void)  /* change command line options from Hugs command line */
{
  String s;
  if ((s=readFilename())!=0) {
    do {
      if (!processOption(s)) {
	ERRMSG(0) "Option string must begin with `+' or `-'"
	  EEND;
      }
    } while ((s=readFilename())!=0);
#if USE_REGISTRY
    writeRegString("Options", optionsToStr());
#endif
  } else {
    optionInfo();
  }
}

static Void local toggleSet(c,state)    /* Set command line toggle         */
Char c;
Bool state; {
    Int i;
    for (i=0; toggle[i].c; ++i)
	if (toggle[i].c == c) {
	    *toggle[i].flag = state;
	    return;
	}
    Printf("Warning: unknown toggle `%c'; ignoring.\n", c);
}

static Void local togglesIn(state)      /* Print current list of toggles in*/
Bool state; {                           /* given state                     */
    Int count = 0;
    Int i;
    for (i=0; toggle[i].c; ++i)
#if HASKELL_98_ONLY
	if (*toggle[i].flag == state) {
#else
	if (*toggle[i].flag == state && (!haskell98 || toggle[i].h98)) {
#endif        
	    if (count==0)
		Putchar((char)(state ? '+' : '-'));
	    Putchar(toggle[i].c);
	    count++;
	}
    if (count>0)
	Putchar(' ');
}

Void optionInfo(Void) {                 /* Print information about command */
    static String fmts = "%-5s%s\n";    /* line settings                   */
    static String fmtc = "%-5c%s\n";
    Int    i;

    Printf("TOGGLES: groups begin with +/- to turn options on/off resp.\n");
    for (i=0; toggle[i].c; ++i) {
#if !HASKELL_98_ONLY
	if (!haskell98 || toggle[i].h98) {
#endif
	    Printf(fmtc,toggle[i].c,toggle[i].description);
#if !HASKELL_98_ONLY
	}
#endif
    }

    Printf("\nOTHER OPTIONS: (leading + or - makes no difference)\n");
    Printf(fmts,"hnum","Set heap size (cannot be changed within Hugs)");
    Printf(fmts,"pstr","Set prompt string to str");
    Printf(fmts,"rstr","Set repeat last expression string to str");
    Printf(fmts,"Pstr","Set search path for modules to str");
    Printf(fmts,"Sstr","Set list of source file suffixes to str");
    Printf(fmts,"Estr","Use editor setting given by str");
    Printf(fmts,"cnum","Set constraint cutoff limit");
#if SUPPORT_PREPROCESSOR
    Printf(fmts,"Fstr","Set preprocessor filter to str");
#endif
#if PROFILING
    Printf(fmts,"dnum","Gather profiling statistics every <num> reductions\n");
#endif

    Printf("\nCurrent settings: ");
    togglesIn(TRUE);
    togglesIn(FALSE);
    Printf("-h%d",heapSize);
    Printf(" -p");
    printString(prompt);
    Printf(" -r");
    printString(repeatStr);
    Printf(" -c%d",cutoff);
    Printf("\nSearch path     : -P");
    printString(hugsPath);
#if __MWERKS__ && macintosh
    Printf("\n{Hugs}          : %s",hugsdir());
    Printf("\n{Current}       : %s",currentDir());
#endif
    Printf("\nSource suffixes : -S");
    printString(hugsSuffixes);
    Printf("\nEditor setting  : -E");
    printString(hugsEdit);
#if SUPPORT_PREPROCESSOR
    Printf("\nPreprocessor    : -F");
    printString(preprocessor);
#endif
#if PROFILING
    Printf("\nProfile interval: -d%d", profiling ? profInterval : 0);
#endif
#if HASKELL_98_ONLY
    Printf("\nCompatibility   : Haskell 98");
#else
    Printf("\nCompatibility   : %s", haskell98 ? "Haskell 98 (+98)"
					       : "Hugs Extensions (-98)");
#endif
    Putchar('\n');
}

/* Get rid off superfluous trailing space(s) */
#define TRIMSPC() while (*(next-1) == ' ') { next--; charsLeft++; }

#define PUTC(c)                         \
    if (charsLeft > 1) {                \
      *next++=(c);charsLeft--;          \
    } else {                            \
      *next='\0';                       \
    }

#define PUTS(s)                         \
    do { String sref = (s);             \
         Int len = strlen(sref);        \
         if ( charsLeft > len ) {       \
            strcpy(next,sref);          \
            next+=len;                  \
            charsLeft -= len;           \
         } else {                       \
            *next = '\0';               \
	 }                              \
    } while(0)

#define PUTInt(optc,i)                  \
    if ( charsLeft > 20 /*conservative*/ ) { \
      sprintf(next,"-%c%d",optc,i);     \
      next+=strlen(next);               \
    } else {                            \
      *next = '\0';                     \
    }

#define PUTStr(c,s)                     \
    next=PUTStr_aux(next,&charsLeft,c,s)

static String local PUTStr_aux Args((String,Int*,Char, String));

static String local PUTStr_aux(next,chLeft,c,s)
String next;
Int*   chLeft;
Char   c;
String s; {
    Int charsLeft = *chLeft;
    Int len;
    if (s && (len = strlen(s)) > 0 ) {
	String t = 0;
	len = strlen(s);

	if ( (Int)(len + 10) > charsLeft ) {
	    *next = '\0';
	    /* optionsToStr() will not to break off immediately,
	     * but soon enough. 
	     */
	    return next;
	}

	*next++ = '-';
	*next++=c;
	*next++='"';
	charsLeft -= 3;
	/* 
	 * Subtlety if *s == '\0': for-loop below bails out right away,
	 * causing strlen(next) to report quite possibly an inappropriate 
	 * result. => always zero-terminate first.
	 *
	 * Do this even if we now explicitly test for 's' not being "" at
	 * the start.
	 */
	*next = '\0';
	for(t=s; *t; ) {
	    /* Explicitly bind result to a local to avoid 
	     * duplicating work within PUTS() macro. Ugly.
	     */
	    String strChar = unlexChar(ExtractChar(t),'"');
	    PUTS(strChar);
	}
	next+=strlen(next);
	PUTS("\" ");
    }
    *chLeft = charsLeft;
    return next;
}

String optionsToStr() {          /* convert options to string */
    static char buffer[2000];
    String next = buffer;
    Int charsLeft = 2000;

    Int i;
    for (i=0; toggle[i].c; ++i) {
	PUTC(*toggle[i].flag ? '+' : '-');
	PUTC(toggle[i].c);
	PUTC(' ');
    }
#if !HASKELL_98_ONLY
    PUTS(haskell98 ? "+98 " : "-98 ");
#endif
    PUTInt('h',hpSize);  PUTC(' ');
    PUTStr('p',prompt);
    PUTStr('r',repeatStr);
    PUTStr('P',hugsPath);
    PUTStr('S',hugsSuffixes);
    PUTStr('E',hugsEdit);
    PUTInt('c',cutoff);  PUTC(' ');
#if SUPPORT_PREPROCESSOR
    PUTStr('F',preprocessor);
#endif
#if PROFILING
    PUTInt('d',profiling ? profInterval : 0);
#endif
    TRIMSPC();
    PUTC('\0');

    return buffer;
}


#undef TRIMSPC
#undef PUTC
#undef PUTS
#undef PUTInt
#undef PUTStr

/* --------------------------------------------------------------------------
 * Reading and processing option strings:
 * ------------------------------------------------------------------------*/

Void readOptions(options,freeUp)  /* read options from string */
String options;
Bool   freeUp; {
    if (!readOptions2(options)) {
        ERRMSG(0) "Option string must begin with `+' or `-'"
	EEND;
    }
    if (options && freeUp) {
	free(options);
    }
}

Bool readOptions2(options)         /* read options from string */
String options; {
    String s;
    if (options) {
	stringInput(options);
	while ((s=readFilename())!=0) {
	    if (*s && !processOption(s))
		return FALSE;
	}
    }
    return TRUE;
}

Bool processOption(s)    /* process string s for options,   */
String s; {              /* return FALSE if none found.     */
    Bool state;

    if (s[0]=='-')
	state = FALSE;
    else if (s[0]=='+')
	state = TRUE;
    else
	return FALSE;

    while (*++s)
	switch (*s) {
	    case 'p' : if (s[1]) {
			   if (prompt) free(prompt);
			   prompt = strCopy(s+1);
		       }
		       return TRUE;

	    case 'r' : if (s[1]) {
			   if (repeatStr) free(repeatStr);
			   repeatStr = strCopy(s+1);
		       }
		       return TRUE;

#if PROFILING
	    case 'd' : {                /* random choice of letter - ADR   */
			   Int i = argToInt(s+1);
			   if (i > 0) {
			       profiling = TRUE;
			       profInterval = i;
			   } else {
			       profiling = FALSE;
			       /* To keep the profiling test efficient(?)
				* we dont actually disable the gathering
				* of profiling statistics - we just gather
				* them very infrequently. ADR
				*/
			       profInterval = MAXPOSINT;
			   }
		       }
		       return TRUE;
#endif

	    case 'P' : {
                           String prelLoc;
 	                   String savedPath;
			   
			   savedPath = hugsPath;
			   if (*(s+1) == '\0') {
			     hugsPath = uniqPath(strCopy(HUGSPATH));
			   } else {
			     hugsPath  = substPath(s+1,hugsPath ? hugsPath : "");
			   }
			   prelLoc = findMPathname(STD_PRELUDE);
			   /* prelLoc points to static storage, don't free. */
			   if (!prelLoc) {
			       Printf("ERROR: unable to locate Prelude along new path: \"%s\" - ignoring it.\n", hugsPath);
			       if (hugsPath) free(hugsPath);
			       hugsPath = savedPath;
			   } else {
			       if (savedPath) free(savedPath);
			   }
			   return TRUE;
		       }

	    case 'S' : {
			   String saveSuffixes = hugsSuffixes;
			   if (*(s+1) == '\0')
			       hugsSuffixes = strCopy(HUGSSUFFIXES);
			   else
			       hugsSuffixes = substPath(s+1,hugsSuffixes);
			   if ( !findMPathname(STD_PRELUDE) ) {
			       Printf("ERROR: unable to locate Prelude with new suffix list: \"%s\" - ignoring it.\n", hugsSuffixes);
			       free(hugsSuffixes);
			       hugsSuffixes = saveSuffixes;
			   } else {
			       free(saveSuffixes);
			   }
			   return TRUE;
		       }

	    case 'E' : if (hugsEdit) free(hugsEdit);
		       hugsEdit = strCopy(s+1);
		       return TRUE;

#if SUPPORT_PREPROCESSOR
	    case 'F' : if (preprocessor) free(preprocessor);
		       preprocessor = strCopy(s+1);
		       if (preprocessor && strlen(preprocessor) == 0) {
			   free(preprocessor);
			   preprocessor = NULL;
		       }
		       return TRUE;
#endif

	    case 'i' : ffiAddCppInclude(s+1);
		       return TRUE;

		       /* re-parse options (useful with #!) */
	    case 'X' : return readOptions2(s+1);

	    case 'h' : setHeapSize(s+1);
		       return TRUE;

	    case 'c' : {   Int cutcand = argToInt(s+1);
			   if (cutcand>=1 && cutcand<=1024)
			       cutoff = cutcand;
		       }
		       return TRUE;

	    /* warnings about obsolete options */

	    case 'e':
	    case 'f':
	    case 'N':
	    case 'W':
	    case 'G' : Printf("ERROR: ignoring obsolete %c%c option.\n",
			  state ? '+' : '-', *s);
		       return TRUE;

	    case 'L' : Printf("ERROR: +L is no longer supported for ffihugs - put the argument (without +L) *after* the module - ignoring it.\n");
		       return TRUE;

        default  :
#if !HASKELL_98_ONLY
	           if (strcmp("98",s)==0) {
		       if (heapBuilt() && (state != haskell98)) {
			   FPrintf(stderr,"Haskell 98 compatibility cannot be changed while the interpreter is running\n");
			   FFlush(stderr);
		       } else {
			   haskell98 = state;
		       }
		       return TRUE;
		   } else {
#endif
		       toggleSet(*s,state);
#if !HASKELL_98_ONLY
		   }
#endif
		   break;
	}
    return TRUE;
}

Bool isOption(s)
String s; {                     /* return TRUE if 's' looks like an option */
  return ( s && (s[0] == '-' || s[0] == '+') );
}

Void setHeapSize(s) 
String s; {
    if (s) {
	hpSize = argToInt(s);
	if (hpSize < MINIMUMHEAP)
	    hpSize = MINIMUMHEAP;
	else if (MAXIMUMHEAP && hpSize > MAXIMUMHEAP)
	    hpSize = MAXIMUMHEAP;
	if (heapBuilt() && hpSize != heapSize) {
#define HEAP_RESIZE_MSG "Change to heap size will not take effect until you rerun Hugs"
#if HUGS_FOR_WINDOWS
	    InfoBox(HEAP_RESIZE_MSG);
#endif
#if USE_REGISTRY
	    FPrintf(stderr,HEAP_RESIZE_MSG "\n");
#else
	    FPrintf(stderr,"Cannot change heap size\n");
#endif
#undef HEAP_RESIZE_MSG
            FFlush(stderr);
	} else {
	    heapSize = hpSize;
	}
    }
}

Int argToInt(s)            /* read integer from argument str  */
String s; {
    Int    n = 0;
    String t = s;

    if (*s=='\0' || !isascii(*s) || !isdigit(*s)) {
	ERRMSG(0) "Missing integer in option setting \"%s\"", t
	EEND;
    }

    do {
	Int d = (*s++) - '0';
	if (n > ((MAXPOSINT - d)/10)) {
	    ERRMSG(0) "Option setting \"%s\" is too large", t
	    EEND;
	}
	n     = 10*n + d;
    } while (isascii(*s) && isdigit(*s));

    if (*s=='K' || *s=='k') {
	if (n > (MAXPOSINT/1000)) {
	    ERRMSG(0) "Option setting \"%s\" is too large", t
	    EEND;
	}
	n *= 1000;
	s++;
    }

#if MAXPOSINT > 1000000                 /* waste of time on 16 bit systems */
    if (*s=='M' || *s=='m') {
	if (n > (MAXPOSINT/1000000)) {
	    ERRMSG(0) "Option setting \"%s\" is too large", t
	    EEND;
	}
	n *= 1000000;
	s++;
    }
#endif

#if MAXPOSINT > 1000000000
    if (*s=='G' || *s=='g') {
	if (n > (MAXPOSINT/1000000000)) {
	    ERRMSG(0) "Option setting \"%s\" is too large", t
	    EEND;
	}
	n *= 1000000000;
	s++;
    }
#endif

    if (*s!='\0') {
	ERRMSG(0) "Unwanted characters after option setting \"%s\"", t
	EEND;
    }

    return n;
}

/* --------------------------------------------------------------------------
 * Process the options entries in an argv-vector:
 * ------------------------------------------------------------------------*/
Void processOptionVector(argc,argv)
Int    argc;
String argv[]; {
    Int i;

    for (i=1; i<argc; ++i) {            /* process command line arguments  */
      if ( argv[i] && argv[i][0] ) {
	  /* Willfully ignore the bool returned by processOption();
	   * non-options (i.e., scripts) will be handled later on.
	   */
	  processOption(argv[i]);
      }
    }
}

/*
 * Read option settings from sources other than the command-line:
 *
 *   - HUGSFLAGS environment variable
 *   - Registry (machine-wide and user-specific portions.)
 *   - Preference files (MacOS<t>, where t < 10).
 *   - WinHugs GUI options.
 *
 */
Void readOptionSettings() {
#if USE_PREFERENCES_FILE
    FILE *f;
    FileName hugsPrefsFile = "\0";
#endif

#if USE_REGISTRY
    readOptions(readRegString(HKEY_LOCAL_MACHINE,hugsRegRoot,"Options",""), TRUE);
    if (!fromEnv("IGNORE_USER_REGISTRY",NULL)) {
      /* If IGNORE_USER_REGISTRY exist as an env var, don't consult
       * the user portion of the Registry. Emergency workaround if it has
       * somehow become invalid.
       */
      readOptions(readRegString(HKEY_CURRENT_USER, hugsRegRoot,"Options",""), TRUE);
    }
#endif /* USE_REGISTRY */
#if USE_PREFERENCES_FILE
    if (f=fopen(PREFS_FILE_NAME,"r")) {
           /* is preferences file in the {Current} folder? */
	  readPrefsFile(f);
	} else {
          /* is preferences file in the {Hugs} folder? */
        strcpy(hugsPrefsFile,macHugsDir);
        strcat(hugsPrefsFile,":");
        strcat(hugsPrefsFile,PREFS_FILE_NAME);
        if (f=fopen(hugsPrefsFile,"r"))
          readPrefsFile(f);
	  } /* else: take default preferences */
    readOptions(hugsFlags,FALSE);
#else
    readOptions(fromEnv("HUGSFLAGS",""),FALSE);
#endif
}

#if USE_PREFERENCES_FILE
static Void readPrefsFile(FILE *f)
{ GVarname line  = "";
  int      linep = 0;

  char c;
      
  while ( (c=fgetc(f)) != EOF && c != '\n') {     /* read HUGSFLAGS          */
    if ((c != '\t') && (c != '\r')) {             /* skip some control chars */
      line[linep++] = c;
      line[linep]   = '\0';
    }
  }
  strcpy(hugsFlags,line);
    
  iniArgc = 0;
  do  {                                  /* read input command line files   */
    while ((c == '\n') || (c == '\t') || (c == ' '))  /* skip blank spaces  */
      c=fgetc(f);    
    if (c == '"') {                      /* filename found                  */
      linep = 0;
      iniArgv[iniArgc][0] = '\0';
      while ((c=fgetc(f)) != EOF && c != '"') {
        if (linep <= 32) {              /* filename limit on a mac 32 chars */
          iniArgv[iniArgc][linep++] = c;
          iniArgv[iniArgc][linep]   = '\0';
        }
      }
      if (c == EOF) {
        ERRMSG(0) "Incorrect name specification in preferences file"
		EEND;
      } else {
		  iniArgc++;
	    }
    }
  } while ( (c = fgetc(f)) != EOF );
}
#endif
