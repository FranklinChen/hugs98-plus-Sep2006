/* --------------------------------------------------------------------------
 * Error handling support functions
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: errors.h,v $
 * $Revision: 1.13 $
 * $Date: 2005/09/01 14:57:50 $
 * ------------------------------------------------------------------------*/
#ifndef __ERRORS_H__
#define __ERRORS_H__

extern Void internal	    Args((String)) HUGS_noreturn;
extern Void fatal	    Args((String)) HUGS_noreturn;
extern Void stopAnyPrinting Args((Void));

/*---------------------------------------------------------------------------
 * Compiler output
 * Tweaking this lets us redirect prompts, error messages, etc - but has no
 * effect on output of Haskell programs (which should use hPutStr and friends).
 *-------------------------------------------------------------------------*/

#if REDIRECT_OUTPUT
extern Void   hugsPrintf            Args((const char *, ...));
extern Void   hugsPutchar           Args((int));
extern Void   hugsFlushStdout       Args((Void));
extern Void   hugsEnableOutput      Args((Bool));
extern String hugsClearOutputBuffer Args((Void));

extern Void   hugsFFlush    	    Args((FILE*));
extern Void   hugsFPrintf   	    Args((FILE*, const char*, ...));
extern Void   hugsPutc      	    Args((int, FILE*));

#define Printf         	     hugsPrintf
#define Putchar        	     hugsPutchar
#define FlushStdout    	     hugsFlushStdout
#define EnableOutput   	     hugsEnableOutput
#define ClearOutputBuffer    hugsClearOutputBuffer

#define FFlush               hugsFFlush
#define FPrintf              hugsFPrintf
#define Putc                 hugsPutc

#else /* !REDIRECT_OUTPUT */

#define Printf               printf
#define Putchar              putchar
#define FlushStdout()        fflush(stdout)
#define EnableOutput(f)      doNothing()
#define ClearOutputBuffer()  0

#define FFlush               fflush
#define FPrintf              fprintf
#define Putc                 putc
#endif /* REDIRECT_OUTPUT */

/*-------------------------------------------------------------------------*/

#if HUGS_FOR_WINDOWS
/* output to stderr uses RED color already */
#undef  Hilite
#undef  Lolite
#define Hilite()         doNothing()
#define Lolite()         doNothing()
#define SetForeColor(c)  WinHugsColor(c);
#define errorStream	 stderr
#else
#define Hilite()         doNothing()
#define Lolite()         doNothing()
#define errorStream	 stdout
#endif

#define ERRMSG(l)	 Hilite(); errHead(l); FPrintf(errorStream,
#define EEND       	 ); Lolite(); errFail()
#define EEND_NORET       ); Lolite()
#define ETHEN		 );
#define ERRTEXT		 Hilite(); FPrintf(errorStream,
#define ERREXPR(e)	 Hilite(); printExp(errorStream,e); Lolite()
#define ERRTYPE(e)	 Hilite(); printType(errorStream,e); Lolite()
#define ERRCONTEXT(qs)   Hilite(); printContext(errorStream,qs); Lolite()
#define ERRPRED(pi)      Hilite(); printPred(errorStream,pi); Lolite()
#define ERRKIND(k)	 Hilite(); printKind(errorStream,k); Lolite()
#define ERRKINDS(ks)	 Hilite(); printKinds(errorStream,ks); Lolite()
#define ERRFD(fd)	 Hilite(); printFD(errorStream,fd); Lolite()

extern Void errHead      Args((Int));		   /* in main.c		   */
extern Void errFail	 Args((Void)) HUGS_noreturn;
extern Void errAbort	 Args((Void));

extern sigProto(breakHandler);

extern Bool breakOn      Args((Bool));		   /* in machdep.c	   */

#include <setjmp.h>
extern jmp_buf catch_error;          /* jump buffer for error trapping  */

/*-------------------------------------------------------------------------*/

#endif /* __ERRORS_H__ */
