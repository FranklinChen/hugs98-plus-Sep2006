/* --------------------------------------------------------------------------
 * Basic data type definitions, prototypes and standard macros including
 * machine dependent variations...
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: prelude.h,v $
 * $Revision: 1.80 $
 * $Date: 2005/09/11 00:33:00 $
 * ------------------------------------------------------------------------*/
#ifndef __PRELUDE_H__
#define __PRELUDE_H__

#include "config.h"
#include "options.h"
#include <stdio.h>

#if HAVE_STDLIB_H
# include <stdlib.h>
#endif
#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#if HAVE_UNISTD_H
# include <unistd.h>
#endif

/*---------------------------------------------------------------------------
 * Most of the configuration code from earlier versions of Hugs has been moved
 * into config.h (which is usually automatically generated).  
 *
 * Most of the configuration code is "feature based".  That is, the 
 * configure script looks to see if a particular feature (or misfeature)
 * is present on the compiler/OS.  
 *
 * A small amount of configuration code is still "system based": it tests
 * flags to determine what kind of compiler/system it's running on - from
 * which it infers what features the compiler/system has.  Use of system
 * based tests generally indicates that we can't remember/figure out
 * what the original problem was and so we can't add an appropriate feature
 * test to the configure script.
 *-------------------------------------------------------------------------*/

#ifdef __RISCOS__ /* Acorn DesktopC running RISCOS2 or 3 */
# define RISCOS 1
#else
# define RISCOS 0
#endif

#if defined __DJGPP__ && __DJGPP__==2
# define DJGPP2 1
#else
# define DJGPP2 0
#endif

#if defined __MSDOS__ && __MSDOS__ && !DJGPP2
# define DOS 1
#else
# define DOS 0
#endif

#ifdef __SYMBIAN32__
#define IS_WIN32 0
#define IS_WINDOWS 0
#else
#if defined(_WIN32) || defined(__WIN32__)
# define IS_WIN32 1
# define IS_WINDOWS 1
#elif defined(_WIN64)
# define IS_WIN64 1
# define IS_WINDOWS 1
#else
# define IS_WIN32 0
# define IS_WINDOWS 0
#endif
#endif

/* using a (possibly multi-byte) encoding of Chars in Strings and I/O? */
#if CHAR_ENCODING_LOCALE || CHAR_ENCODING_UTF8
# define CHAR_ENCODING 1
#endif

#if defined(_WIN32) || cygwin32_HOST_OS
#define STDCALL_SUPPORTED 1
#endif

/*---------------------------------------------------------------------------
 * Configuration options
 *
 * Most configuration options are arguments to the configure script
 * (try running "configure --help").  The following options are either
 * experimental or require changes to "Prelude.hs", the standard libraries
 * and demos and therefore cannot be modified using the configure script.
 * Most users should leave them alone!
 *
 *   OBSERVATIONS   to include support for `observe' and friends
 *   TREX	    to include support for Typed Rows and EXtensions.
 *   IPARAM	    to include support for Implicit Parameters.
 *   MUDO	    to include support for Recursive-do notation
 *   ZIP_COMP       to include support for Zip Comprehensions
 *   MULTI_INST	    to include support for Multi-Instance Resolution.
 *   HASKELL_ARRAYS to include support for Haskell array primitives.
 *   IO_MONAD	    to include the IO monad primitives and support.
 *   IO_HANDLES     to include the IO file operations.
 *   IO_REFS	    Ref type for IO_MONAD, and simple operations.
 *   FLUSHEVERY	    to force a fflush after every char in putStr/hPutStr.
 *   NPLUSK	    to include support for (n+k) patterns.
 *   BIGNUMS	    to include support for Integer bignums.
 *   FIXED_SUBST    to force a fixed size for the current substitution.
 *   DYN_TABLES	    to allocate tables dynamically, currently just a memory
 *		    saving trick, but this may be extended at a later stage
 *		    to allow at least some of the tables to be extended
 *		    dynamically at run-time to avoid exhausted space errors.
 *   GC_STABLEPTRS  to include support for safely passing Haskell
 *                  pointers over to C
 *                  (only required if you use callbacks in the foreign
 *                  language interface)
 *   GC_MALLOCPTRS  to include support for automatic deallocation of 
 *                  C objects when Haskell is done with them.
 *   GC_WEAKPTRS    to include support for weak pointers.
 *   STABLE_NAMES   stable names a la Simon PJ
 *   MONAD_COMPS    to allow monad comprehensions.
 *   REDIRECT_OUTPUT ability to redirect stdout/stderr to a buffer.
 *                  Only necessary for the Hugs server interface
 *                  (which is used in the Netscape plugin and the standalone
 *                  evaluator). 
 *   WORD_OPS       to include operations on unsigned ints
 *   ADDR_OPS       to include operations on addresses
 *   PROVIDE_INT64  to include 64 bit Ints
 *   SHORT_CIRCUIT_COERCIONS to try to apply these rewrites at runtime:
 *                    integerToInt (intToInteger x) -> x
 *                    rationalToFloat  (fromDouble {dict} x) -> doubleToFloat x
 *                    rationalToDouble (fromDouble {dict} x) -> x
 *   FAST_WHATIS    to use a macro instead of a func for whatIs(), for speed
 *   FAST_WHATIS1   to use whatIs1(), a faster version of whatIs(), for speed
 *-------------------------------------------------------------------------*/

#if !HASKELL_98_ONLY
#define TREX		1
#define IPARAM		1
#define MUDO		1
#define OBSERVATIONS    1
#define ZIP_COMP	1
#define HERE_DOC	1
#else
#define TREX            0
#define IPARAM          0
#define MUDO		0
#define OBSERVATIONS    0
#define ZIP_COMP	0
#define HERE_DOC	0
#endif
#define HASKELL_ARRAYS	 1
#define IO_MONAD	 1
#define IO_HANDLES       1
#define IO_REFS		 1 /* Experimental IO Ref type			   */
#define FLUSHEVERY	 1
#define NPLUSK		 1 /* Warning: There are those that would prefer 0 */
#define BIGNUMS		 1 /* Experimental bignum implementation           */
#define FIXED_SUBST	 0 /* Warning: This may not be appropriate for PCs */
#define DYN_TABLES	 SMALL_HUGS /* For dynamically allocated tables	   */
#define GC_STABLEPTRS    1 /* May be required by external libraries        */
#define GC_MALLOCPTRS    1 /* May be required by external libraries        */
#define GC_WEAKPTRS      1
#define STABLE_NAMES     1
#define MONAD_COMPS	 0
#define REDIRECT_OUTPUT  (!HUGS_FOR_WINDOWS)
#define WORD_OPS         1
#define ADDR_OPS         1
#define PROVIDE_INT64    1
#define TIME_MODULE      1
#define DIRECTORY_MODULE 1
#define MULTI_LINEFEED   1 /* Platform-independent linefeed handling        */
#define MULTI_INST       0
#define WANT_FIXED_SIZE_TABLES 0 /* use fixed-size tables for internal structs */
                                 /* (as opposed to dynamically growable ones)  */

#define UNICODE_CHARS    1 /* Char is Unicode (ISO-10646) */

#define SHORT_CIRCUIT_COERCIONS 1

#define FAST_WHATIS      1
#define FAST_WHATIS1     1	 /* can only use if FAST_WHATIS is 1 */

/*---------------------------------------------------------------------------
 * Platform-dependent settings:
 *-------------------------------------------------------------------------*/

#if HAVE_MACSYSTEM	/* Macintosh system() prototype. */
int macsystem(char *filenames);
#endif


/*---------------------------------------------------------------------------
 * Include stuff required for WinHugs:
 *  mainly redirect get/put console functions
 *  also required API definitions
 *-------------------------------------------------------------------------*/

#if HUGS_FOR_WINDOWS
# include "winhugs/Winhugs.h"
#endif


/*---------------------------------------------------------------------------
 * Macros used in declarations:
 *  function prototypes
 *  local/far declarations
 *  HUGS_noreturn/HUGS_unused (prevent spurious warnings)
 *  result type of main
 *  dynamic linking declarations
 *-------------------------------------------------------------------------*/

#if PROTOTYPES		  /* To enable use of prototypes whenever possible */
#define Args(x) x
#else
#define Args(x) ()
#endif

/* local = prefix for locally defined functions */
/* far   = prefix for far pointers              */
#if DOS
# define local near pascal
#else
# define local
# define far
#endif

#ifdef __GNUC__     /* Avoid spurious warnings                             */
#if __GNUC__ > 2 || __GNUC__ == 2 && __GNUC_MINOR__ >= 7
#define HUGS_noreturn  __attribute__ ((noreturn))
#define HUGS_unused    __attribute__ ((unused))
#else
#define HUGS_noreturn  
#define HUGS_unused
#endif
#else
#define HUGS_noreturn  
#define HUGS_unused
#endif

/*---------------------------------------------------------------------------
 * Dynamic linking tricks
 *-------------------------------------------------------------------------*/

#if 0
/* DLLs, shareable libraries, etc generated by the foreign language
 * interface generator need some way to access the Hugs stack, standard
 * constructor functions, the garbage collector, etc.
 *
 * Most UNIX systems use the same mechanisms as for static linking - when
 * you load the shareable object file, it patches it with the values of
 * the required symbols.
 *
 * DOS/Windows uses a different mechanism - a DLL (or EXE) accesses code and
 * data from other DLLs (or EXEs) via an indirection.  No big deal for code
 * but it makes a huge difference when accessing data and the compiler
 * _HAS TO KNOW_ whether a piece of data is accessed directly (it's in
 * the same DLL/EXE) or indirectly.
 *
 * On Microsoft Visual C++, this is done using a VC++ specific language
 * extension on declarations and definitions of all imported/exported
 * symbols.  The "extern" declarations of imported symbols are modified
 * with "__declspec(dllimport)" and the definitions of exported symbols
 * are marked with "__declspec(dllexport)".  If you want both the 
 * declaration and the definition to coexist in the same file without 
 * generating warning messages, you have to go through contortions.
 *
 * We also do this under MinGW, to interoperate with MSVC.
 *
 * Sigh, to add to the confusion, MS C and Borland C disagree about whether
 * to put the export declaration before or after the return type - so we
 * have to parameterise it to allow both.
 */
#endif

#if defined(__BORLANDC__)
# define DLLIMPORT(rty) rty far _import
# define DLLEXPORT(rty) rty far _export
#elif defined(_WIN32) /* Microsoft Windows */
# define DLLIMPORT(rty) __declspec(dllimport) rty
# define DLLEXPORT(rty) __declspec(dllexport) rty
#else 
# define DLLIMPORT(rty) rty
# define DLLEXPORT(rty) rty
#endif /* Don't need to declare DLL exports */

#ifdef __EXTERNAL
#define HUGSAPI(rty) DLLIMPORT(rty)
#else
#define HUGSAPI(rty) DLLEXPORT(rty)
#endif

/*---------------------------------------------------------------------------
 * String operations:
 *-------------------------------------------------------------------------*/

#if HAVE_STRING_H
# include <string.h>
#else
extern int      strcmp     Args((const char*, const char*));
extern int      strncmp    Args((const char*, const char*, int));
extern char     *strchr    Args((const char*, int));
extern char     *strrchr   Args((const char*, int));
extern size_t   strlen     Args((const char *));
extern char	*strcpy	   Args((char *, const char*));
extern char     *strcat	   Args((char *, const char*));
#endif
#if HAVE_STRCMP
#if HUGS_FOR_WINDOWS
#define strCompare stricmp
#else
#define strCompare strcmp
#endif
#else /* probably only used for DOS - ADR */
extern  int     stricmp	   Args((const char *, const char*));
#define strCompare stricmp
#endif

#if HAVE_CTYPE_H
# include <ctype.h>
#endif
#ifndef isascii
#define  isascii(c)	(((unsigned)(c))<128)
#endif

/*---------------------------------------------------------------------------
 * Printf-related operations:
 *-------------------------------------------------------------------------*/

#if HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#if !HAVE_SNPRINTF
# if HAVE__SNPRINTF
#  define snprintf _snprintf
# elif __MWERKS__ && macintosh
extern int snprintf  Args((char*, unsigned long, const char*, va_list));
# else
extern int snprintf   Args((char*, size_t, const char*, ...));
# endif
#endif

#if !HAVE_VSNPRINTF
# if __MWERKS__ && macintosh
extern int vsnprintf  Args((char*, unsigned long, const char*, va_list));
# else
extern int vsnprintf  Args((char*, size_t, const char*, va_list));
# endif
#endif

/*---------------------------------------------------------------------------
 * Pipe-related operations:
 *
 * On Windows, many standard Unix names acquire a leading underscore.
 * Irritating, but easy to work around.
 *-------------------------------------------------------------------------*/

#if !HAVE_POPEN && HAVE__POPEN
#define popen(x,y) _popen(x,y)
#endif
#if !HAVE_PCLOSE && HAVE__PCLOSE
#define pclose(x) _pclose(x)
#endif

/*---------------------------------------------------------------------------
 * Interrupting execution (signals, allowBreak):
 *-------------------------------------------------------------------------*/

#if !DOS && VOID_INT_SIGNALS
# define sigProto(nm)	void nm Args((int))
# define sigRaise(nm)	nm(1)
# define sigHandler(nm)	void nm(sig_arg) int sig_arg;
# define sigResume	return
#else
# define sigProto(nm)	int nm Args((Void))
# define sigRaise(nm)	nm()
# define sigHandler(nm)	int nm(Void)
# define sigResume	return 1
#endif

/* allowBreak: call to allow user to interrupt computation
 * ctrlbrk:    set control break handler
 */

/* On Unix (and almost every other system), the interrupt handlers perform
 * a longjmp to break out of the current computation.
 * On Win32 this does not work because the interrupt handler is run in
 * a separate thread from the main computation.  Instead we set a 
 * flag (the global variable "broken") to request an interrupt and
 * all potentially infinite loops of the evaluator check the flag using
 * the "allowBreak" call.
 */ 
#define HANDLERS_CANT_LONGJMP IS_WINDOWS


#if DOS

# define allowBreak()	;

#elif HANDLERS_CANT_LONGJMP /* eg Win32 */

# define ctrlbrk(bh)	do { signal(SIGINT,bh); signal(SIGBREAK,bh); } while (0)
# define allowBreak()	if (broken) { broken = FALSE; sigRaise(breakHandler); }

#else /* !DOS && !HANDLERS_CANT_LONGJMP - eg Unix */

# if HAVE_SIGPROCMASK
#  include <signal.h>
#  define ctrlbrk(bh)	{ sigset_t mask; \
			  signal(SIGINT,bh); \
			  sigemptyset(&mask); \
			  sigaddset(&mask, SIGINT); \
			  sigprocmask(SIG_UNBLOCK, &mask, NULL); \
			}
# else
#  define ctrlbrk(bh)	signal(SIGINT,bh)
# endif
#if __MWERKS__ && macintosh
# define allowBreak()   doNothing()
#else
# define allowBreak()   doNothing()
#endif

#endif /* !DOS && !HANDLERS_CANT_LONGJMP */


#ifndef SIGBREAK /* Sigh, not defined in cygwin32 beta release 16 */
# define SIGBREAK 21
#endif

/*---------------------------------------------------------------------------
 * Floating point support
 *-------------------------------------------------------------------------*/

/* Can we fit floats into ints? */
#define BREAK_FLOATS (SIZEOF_FLOAT > SIZEOF_INT)

#if  FLOATS_SUPPORTED

#define FloatImpType	   float
#define FloatPro	   double  /* type to use in prototypes		   */
				   /* strictly ansi (i.e. gcc) conforming  */
				   /* but breaks data hiding :-(	   */
#define FloatFMT	   "%.7g"

/* Is double too big for two ints?  (if so, use float instead) */
#define DOUBLE_IS_FLOAT (SIZEOF_DOUBLE > 2*SIZEOF_INT)

#if DOUBLE_IS_FLOAT
#define DoubleImpType	   FloatImpType
#define DoublePro	   FloatPro
#define DoubleFMT          FloatFMT
#else
#define DoubleImpType	   double
#define DoublePro	   double
#define DoubleFMT          "%.15g"
#endif

#if HAVE_FLOAT_H

#include <float.h>

# define HUGS_FLT_RADIX    FLT_RADIX
# define HUGS_FLT_MANT_DIG FLT_MANT_DIG
# define HUGS_FLT_MIN_EXP  FLT_MIN_EXP
# define HUGS_FLT_MAX_EXP  FLT_MAX_EXP

#if DOUBLE_IS_FLOAT
# define HUGS_DBL_MANT_DIG HUGS_FLT_MANT_DIG
# define HUGS_DBL_MIN_EXP  HUGS_FLT_MIN_EXP
# define HUGS_DBL_MAX_EXP  HUGS_FLT_MAX_EXP
#else
# define HUGS_DBL_MANT_DIG DBL_MANT_DIG
# define HUGS_DBL_MIN_EXP  DBL_MIN_EXP
# define HUGS_DBL_MAX_EXP  DBL_MAX_EXP
#endif

#elif HAVE_VALUES_H

#include <values.h>

# define HUGS_FLT_RADIX    _EXPBASE
# define HUGS_FLT_MANT_DIG FSIGNIF
# define HUGS_FLT_MIN_EXP  FMINEXP
# define HUGS_FLT_MAX_EXP  FMAXEXP 

#if DOUBLE_IS_FLOAT
# define HUGS_DBL_MANT_DIG HUGS_FLT_MANT_DIG
# define HUGS_DBL_MIN_EXP  HUGS_FLT_MIN_EXP
# define HUGS_DBL_MAX_EXP  HUGS_FLT_MAX_EXP
#else
# define HUGS_DBL_MANT_DIG DSIGNIF
# define HUGS_DBL_MIN_EXP  DMINEXP
# define HUGS_DBL_MAX_EXP  DMAXEXP
#endif

#endif

#ifdef __SYMBIAN32__
/* Guesswork, really */
#define HUGS_FLT_RADIX        2
#define HUGS_FLT_MANT_DIG    24
#define HUGS_FLT_MIN_EXP   -125
#define HUGS_FLT_MAX_EXP    128
#define HUGS_DBL_MANT_DIG    53
#define HUGS_DBL_MIN_EXP  -1021
#define HUGS_DBL_MAX_EXP   1024
#endif

#else /* !FLOATS_SUPPORTED */

#define FloatImpType	   int     /*dummy*/
#define FloatPro	   int
#define FloatFMT	   "%d"
#define DoubleImpType	   int     /*dummy*/
#define DoublePro	   int
#define DoubleFMT	   "%d"

#endif /* !FLOATS_SUPPORTED */

/*---------------------------------------------------------------------------
 * Memory allocation
 *-------------------------------------------------------------------------*/

#if HAVE_FARCALLOC
# include <alloc.h>
# define farCalloc(n,s)	farcalloc((unsigned long)n,(unsigned long)s)
#elif HAVE_VALLOC
# define farCalloc(n,s)	(Void *)valloc(((unsigned)n)*((unsigned)s))
#else
# define farCalloc(n,s)	(Void *)calloc(((unsigned)n),((unsigned)s))
#endif

/* bison-generated parsers like to have alloca - so try to define it */

#if HAVE_ALLOCA_H
#include <alloca.h>
#else
# if HAVE__ALLOCA && !defined(__SYMBIAN32__)
#  include <malloc.h>
# endif
#endif


/*---------------------------------------------------------------------------
 * Assertions
 *-------------------------------------------------------------------------*/

#if HAVE_ASSERT_H
#include <assert.h>
#else
#define assert(x) doNothing()
#endif

/*---------------------------------------------------------------------------
 * Preprocessor support
 *-------------------------------------------------------------------------*/

/*
 * Note: initially defined as
 *   #define SUPPORT_PREPROCESSOR USE_PREPROCESSOR && (defined(HAVE_POPEN) || defined(HAVE__POPEN))
 *
 * which worked OK with GNU cpp, but MSVC didn't expand the 'defined' correctly.
 *
 */
#if USE_PREPROCESSOR
# if HAVE_POPEN || HAVE__POPEN
#  define SUPPORT_PREPROCESSOR 1
# else
#  define SUPPORT_PREPROCESSOR 0
# endif
#else
# define SUPPORT_PREPROCESSOR 0
#endif

/*---------------------------------------------------------------------------
 * Environment variables and the registry
 *-------------------------------------------------------------------------*/

/* On Win32 we can use the registry to supplement info in environment 
 * variables.
 */
#define USE_REGISTRY (HAVE_WINDOWS_H && !__MSDOS__)

/*---------------------------------------------------------------------------
 * File operations:
 *-------------------------------------------------------------------------*/

#if !HAVE_UNISTD_H && !_MSC_VER
extern int 	chdir 	   Args((const char*));
#endif

#if HAVE_DIRECT_H
#include <direct.h>
#endif

#ifndef HAVE_STDLIB_H
extern int      system	   Args((const char *));
extern double   atof	   Args((const char *));
extern void     exit       Args((int));
#endif

#ifndef FILENAME_MAX	   /* should already be defined in an ANSI compiler*/
# define FILENAME_MAX 256
#elif   FILENAME_MAX < 256
# undef  FILENAME_MAX
# define FILENAME_MAX 256
#elif   FILENAME_MAX > 8192
	/* Systems with no limit on path length (e.g. the Hurd), will have */
	/* FILENAME_MAX impossibly large.  Ideally we should dynamically   */
	/* allocate/grow these buffers and not use FILENAME_MAX at all.    */
# undef  FILENAME_MAX
# define FILENAME_MAX 8192
#endif

/* Hack, hack: if you have dos.h, you probably have a DOS filesystem */
#ifndef __SYMBIAN32__
/* No dos.h but a DOS filesystem */
#define DOS_FILENAMES              HAVE_DOS_H
#else
#define DOS_FILENAMES 1
#endif
/* ToDo: can we replace this with a feature test? */
#define MAC_FILENAMES              macintosh

#define CASE_INSENSITIVE_FILENAMES (DOS_FILENAMES | RISCOS)

#if CASE_INSENSITIVE_FILENAMES
# if HAVE_STRCASECMP
#  define filenamecmp(s1,s2) strcasecmp(s1,s2)
# elif HAVE__STRICMP
#  define filenamecmp(s1,s2) _stricmp(s1,s2)
# elif HAVE_STRICMP
#  define filenamecmp(s1,s2) stricmp(s1,s2)
# elif HAVE_STRCMPI
#  define filenamecmp(s1,s2) strcmpi(s1,s2)
# endif
#else
# define filenamecmp(s1,s2) strcmp(s1,s2)
#endif

/*---------------------------------------------------------------------------
 * Optimisations:
 *-------------------------------------------------------------------------*/

#ifdef  __GNUC__			/* look for GCC 2.x extensions	   */
#if     __GNUC__ >= 2 && !defined(NeXT)	/* NeXT cc lies and says it's 2.x  */

/* WARNING: if you use the following optimisations to assign registers for
 * particular global variables, you should be very careful to make sure that
 * storage(RESET) is called after a longjump (usually resulting from an error
 * condition) and before you try to access the heap.  The current version of
 * main deals with this using everybody(RESET) at the head of the main read,
 * eval, print loop
 */

#ifdef  m68k				/* global registers on an m68k	   */
#define GLOBALfst	asm("a4")
#define GLOBALsnd	asm("a5")
#define GLOBALsp	asm("a3")
#endif

#ifdef  sparc				/* global registers on a sparc	   */
/* sadly, although the gcc documentation suggests that the following reg   */
/* assignments should be ok, experience shows (at least on Suns) that they */
/* are not -- it seems that atof() and friends spoil things.		   */
/*#define GLOBALfst	asm("g5")*/
/*#define GLOBALsnd	asm("g6")*/
/*#define GLOBALsp	asm("g7")*/
#endif /* sparc */

#endif
#endif /* defined(__GNUC__) */

/*---------------------------------------------------------------------------
 * General settings:
 *-------------------------------------------------------------------------*/

#define Void     void   /* older compilers object to: typedef void Void;   */
#if !defined(_XLIB_H_)  /* clashes with similar declaration in Xlib.h      */
typedef unsigned Bool;
#endif
#define TRUE     1
#define FALSE    0
#ifndef _XtIntrinsic_h
typedef char    *String;
#endif
typedef int      Int;
typedef signed char Int8;
typedef short    Int16;
typedef long     Long;
typedef int      Char;
typedef unsigned Unsigned;
typedef void*    Pointer;

#define doNothing() do { } while (0) /* Null statement */

#ifndef STD_PRELUDE
#if     RISCOS
#define STD_PRELUDE	      "prelude"
#define STD_PRELUDE_HUGS      "hugs.prelude"
#define STD_EMPTY_MODULE      "hugs.prelude"
#else
#define STD_PRELUDE	      "Prelude"
#define STD_PRELUDE_HUGS      "Hugs.Prelude"
#define STD_EMPTY_MODULE      "Hugs"
#endif
#endif

#if IO_MONAD
#define NUM_HANDLES	   40
#endif
#define NUM_TUPLES         100
#define NUM_OFFSETS        1024
#define NUM_LAT1_CHARS     256
#if TREX
#define NUM_EXT		   100
#endif

#if PROFILING
#define DEF_PROFINTDIV	   10		/* hpsize/this cells between samples*/
#endif

#if     SMALL_HUGS			/* the McDonalds mentality :-)	   */
#define Pick(s,r,l)	   s
#endif
#if     REGULAR_HUGS
#define Pick(s,r,l)	   r
#endif
#if     LARGE_HUGS
#define Pick(s,r,l)	   l
#endif

#if OBSERVATIONS
#define NUM_OBS_TAGS       Pick(100,    200,        1000)
#define NUM_BRKPTS         Pick(100,    200,        200)
#endif

#define NUM_SCRIPTS        Pick(64,     200,        800)
#define NUM_FIXUPS         Pick(400,    400,        1000)
#define NUM_TYCON          Pick(60,     320,        2000)
#define NUM_NAME           Pick(1000,   4000,       32000)
#define NUM_CLASSES        Pick(30,     240,        1000)
#define NUM_INSTS          Pick(200,    400,        4000)
#define NUM_TEXT           Pick(12000,  20000,      320000)
#define NUM_TEXTH	   Pick(1,      10,         10)
#define NUM_TYVARS         Pick(800,    2000,       8000)
#define NUM_STACK          Pick(1800,   16000,      160000)
#define NUM_ADDRS          Pick(28000,  120000,     1280000)
#define MINIMUMHEAP	   Pick(7500,   19000,      19000)
#define MAXIMUMHEAP	   Pick(32765,  0,          0)
#define DEFAULTHEAP        Pick(28000,  100000,      1000000)
#define MAXPOSINT          Pick(0x7fff, 0x7fffffff, 0x7fffffff)
#define MAXHUGSWORD        Pick(0xffffU, 0xffffffffU, 0xffffffffU)
#define NUM_STABLEPTRS	   Pick(10,     100,        10000)
#define NUM_MALLOCPTRS	   Pick(10,     100,        10000)
#ifdef DOTNET
#define NUM_DOTNETPTRS	   Pick(10,     100,        10000)
#endif
#define NUM_DTUPLES	   Pick(3,      5,          5)

/* Some infinite computations generate an infinite depth of alternations
 * between eval() and run().  If they don't use the Hugs stack or heap,
 * they will overrun the C stack, crashing the interpreter.  To protect
 * against this, we place a limit on the depth of recursion of eval().
 */
#define MAX_EVAL_DEPTH	   Pick(1024,	4096,	    16384)

/* Representation of Integer: requires BIGBASE == 10^BIGEXP */
#define BIGBASE		   Pick(100,    10000,      10000)
#define BIGEXP		   Pick(2,      4,          4)

#define MINNEGINT          (-MAXPOSINT-1)

#define NUM_MODULE         NUM_SCRIPTS


#if DYN_TABLES				/* Tables may be alloc'd at runtime*/
#define DECTABLE(tab)	   far *tab	/* macros for declaration & defn   */
#define DYNDECTABLE(tab)   DECTABLE(tab)
#define DEFTABLE(tab,sz)   far *tab = 0
#else					/* or at compile-time:		   */
#define DECTABLE(tab)	   tab[]
#define DYNDECTABLE(tab)   far *tab
#define DEFTABLE(tab,sz)   tab[sz]
#endif

#define minRecovery	   Pick(1000,  1000,       1000)
#define bitsPerWord	   Pick(16,    32,         32)
#define wordShift	   Pick(4,     5,          5)
#define wordMask	   Pick(15,    31,         31)

#define bitArraySize(n)    ((n)/bitsPerWord + 1)
#define placeInSet(n)      ((-(n)-1)>>wordShift)
#define maskInSet(n)       (1<<((-(n)-1)&wordMask))

/*-------------------------------------------------------------------------*/

#endif /* __PRELUDE_H__ */
