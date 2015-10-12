/* --------------------------------------------------------------------------
 * Standalone hugs system
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: runhugs.c,v $
 * $Revision: 1.23 $
 * $Date: 2006/04/26 12:26:44 $
 * ------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#define HUGS_SERVER 1

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "machdep.h"
#include "observe.h"
#include "server.h"

#if defined(_MSC_VER) && !defined(_MANAGED)
#include <windows.h>
#endif

extern int  main      Args((int, char**));
static void check     Args((void));
static void loadHugs  Args((int, char**));

static HugsServerAPI* hugs = 0;

static void check() {
    char* err = hugs->clearError();
    if (err) {
	fprintf(stderr,"runhugs: %s\n",err);
	fflush(stderr);
	exit(1);
    }
}

static void loadHugs(argc,argv)
int    argc;
char* argv[]; {
    hugs = initHugsServer(argc,argv);
    if (NULL == hugs) {
	fprintf(stderr,"runhugs: Unable to initialise Hugs (%s)\n", lastError);
	fflush(stderr);
	exit(1);
    }
    hugs->setOutputEnable(0);
    check();
}

/* --------------------------------------------------------------------------
 * main
 * ------------------------------------------------------------------------*/

int main(argc,argv)
int    argc;
char* argv[]; {
    int    exitCode = 0;
    char** hugs_argv;
    int    hugs_argc;
    char*  progname;

    progname = argv ? argv[0] : "runhugs";
    if (!initSystem()) {
      fprintf(stderr,"%s: failed to initialize, exiting\n", progname);
      fflush(stderr);
      exit(1);
    }
#if __MWERKS__ && macintosh
    argc = ccommand(&argv);
#endif

#if defined(FFI_COMPILER)
    generateFFI = TRUE;
#endif

    /* skip over any option flags before the program name */
    {
	int i = 1; /* ignore first arg - name of this program */
	while (i < argc 
	       && argv[i] /* paranoia */
	       && (argv[i][0] == '+' || argv[i][0] == '-')
	       ) {
	    ++i;
	}
	hugs_argv = argv;
	hugs_argc = i;

	argv += i;
	argc -= i;
    }

    if (argc < 1) {
	fprintf(stderr,"%s: missing file argument\n",progname);
	fflush(stderr);
	exit(1);
    }

#if defined(_MSC_VER) && !defined(_MANAGED)
    __try {
#endif

    loadHugs(hugs_argc, hugs_argv);

#if defined(FFI_COMPILER)
    /* all arguments following the module name are passed to the C compiler */
    {
	int i;
	for (i=1; i<argc; ++i)
	    ffiSetFlags(argv[i]);
    }

    hugs->loadFile(argv[0]);
    check();
#else
    /* all arguments following the module name are available via getArgs */
    hugs->loadFile(argv[0]);
    check();

    hugs->setHugsArgs(argc,argv);
    hugs->pushHVal(hugs->compileExpr("Main","main >> return () :: IO ()"));
    exitCode = hugs->doIO();
    check();
#endif

#if defined(_MSC_VER) && !defined(_MANAGED)
    } __except ( ((GetExceptionCode() == EXCEPTION_STACK_OVERFLOW) ? EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH) ) {
      fatal("C stack overflow");
    }
#endif

    shutdownHugsServer(hugs);
    
    exit(exitCode);
    return 0;/*NOTUSED*/
}

#if WANT_TIMER
/* dummy definition: timers are only available in the interpreter */
void updateTimers Args((void));
void updateTimers() {}
#endif
