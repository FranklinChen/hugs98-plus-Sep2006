/*
 * Processing options/toggles.
 *
 *
 */
#ifndef __OPTS_H__
#define __OPTS_H__

/* --------------------------------------------------------------------------
 * Functions for processing and displaying Hugs options:
 * ------------------------------------------------------------------------*/
extern Bool   processOption       Args((String));
extern Void   readOptions         Args((String,Bool));
extern Bool   readOptions2        Args((String));
extern Void   optionInfo          Args((Void));
extern Bool   isOption            Args((String));
extern Void   setOptions          Args((Void));
extern String optionsToStr        Args((Void));
extern Void   processOptionVector Args((Int, String []));
extern Void   readOptionSettings  Args((Void));

extern Void   setLastEdit         Args((String,Int));
extern String getLastEdit         Args((Int*));
extern Int    argToInt            Args((String));
extern Void   setHeapSize         Args((String));

/* --------------------------------------------------------------------------
 * Interpreter flags and options:
 * ------------------------------------------------------------------------*/
extern Bool  showStats;                /* TRUE => print stats after eval  */
extern Bool  addType;                  /* TRUE => print type with value   */
extern Bool  gcMessages;	       /* TRUE => print GC messages	  */
extern Bool  literateScripts;	       /* TRUE => default lit scripts     */
extern Bool  useDots;                  /* TRUE => use dots in progress    */
extern Bool  quiet;                    /* TRUE => don't show progress     */
extern Bool  useQualifiedNames;        /* TRUE => qualify names when printing types and terms */
extern Bool  listScripts;              /* TRUE => list scripts after loading*/
extern Bool  kindExpert;	       /* TRUE => display kind errors in    */
				       /* 	   full detail		    */
extern Bool  allowOverlap;	       /* TRUE => allow overlapping insts   */
extern Bool  allowUnsafeOverlap;       /* TRUE => in addition, allow        */
				       /* potentially inconsistent          */
				       /* overlapping instances             */
extern Bool  useShow;                  /* TRUE => use Text/show printer     */
extern Bool  displayIO;                /* TRUE => use printer for IO result */
extern Bool  printTypeUseDefaults;     /* TRUE => use 'default'ing when printing types */

extern Int   cutoff;		       /* Constraint Cutoff depth	  */
extern String prompt;                  /* Prompt string                   */
extern String repeatStr;               /* Repeat last expr                */
extern String hugsPath;                /* String for file search path     */
extern String hugsSuffixes;            /* Source filename suffixes        */
extern Bool   haskell98;               /* TRUE => Haskell 98 compatibility*/
extern Int    hpSize;                  /* Desired heap size               */
extern String hugsEdit;                /* String for editor command       */

/* The rest are conditionally supported flags: */
#if PROFILING
extern Bool profiling;                 /* TRUE => perform profiling.      */
extern Int  profInterval;              /* interval between samples        */
#endif

#if HUGS_FOR_WINDOWS
extern Bool autoLoadFiles;	       /* TRUE => reload files before eval*/
#endif
#if EXPLAIN_INSTANCE_RESOLUTION
extern Bool  showInstRes;              /* TRUE => show instance resolution */
#endif
#if MULTI_INST
extern Bool  multiInstRes;             /* TRUE => use multi inst resolution */
#endif
#if DEBUG_CODE
extern Bool  debugCode;		        /* TRUE => print G-code to screen  */
#endif
#if DEBUG_SHOWSC
extern Bool  debugSC;			/* TRUE => print SC to screen  */
#endif
#if OBSERVATIONS
extern Bool   rootOpt;                  /* TRUE => enable root optimisation*/
#endif
#if HERE_DOC
extern Bool  hereDocs;                  /* TRUE => enable `here documents' */
#endif

#if HUGS_FOR_WINDOWS
extern Bool  autoLoadFiles;             /* TRUE => automatically reloaded modified files */
#endif

#if SUPPORT_PREPROCESSOR
extern String preprocessor;             /* preprocessor command            */
#endif

extern Bool   printing;                 /* TRUE => currently printing value*/

/* 
 * How to add a new flag / option:
 *
 *  - declare the variable which records the setting of the new option
 *    in the above block; add a definition for it at the start of opts.c,
 *    or local to whatever module the variable is being used.
 *
 *  - decide on what upper/lower-case letter to use for your new option
 *    (getting harder...option handling really ought to be extended with
 *     better support for option strings.)
 *
 *  - if the flag is a toggle, add an entry for it to the toggle[] array
 *    in opts.c. If it is an option that takes an argument, extend
 *    the switch statement in processOption() to cover your option. There's
 *    a decent chance that code for one of the other options supported can 
 *    be adapted to suit your needs..
 * 
 *  - if the code module which uses the new option doesn't already, add
 *    #include "opts.h" to it to bring the underlying variable declaration
 *    into scope.
 */

/*
 * Representing toggles -- exposed here so that UIs (such as winhugs/) can 
 * work off the toggle[] array when putting together an 'Options' dialog.
 *
 */
struct options {                        /* command line option toggles     */
    char   c;                           /* table defined in main app.      */
#if !HASKELL_98_ONLY
    int    h98;                         /* set in Haskell'98 mode?         */
#endif
    String description;
    Bool   *flag;
};

extern struct options toggle[];

#endif /* __OPTS_H__ */
