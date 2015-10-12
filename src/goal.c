/*
 * Keeping track of progress towards a goal.
 */
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "machdep.h"
#include "opts.h"
#include "goal.h"

/* --------------------------------------------------------------------------
 * Display progress towards goal:
 * ------------------------------------------------------------------------*/
static Target currTarget;
static Bool   aiming = FALSE;
static Int    currPos;
static Int    maxPos;
static Int    charCount;

Void setGoal(what, t)                  /* Set goal for what to be t        */
String what;
Target t; {
    if (quiet
#if EXPLAIN_INSTANCE_RESOLUTION
	      || showInstRes
#endif
			    ) return;
    currTarget = (t?t:1);
    aiming     = TRUE;
    if (useDots) {
	currPos = strlen(what);
	maxPos  = getTerminalWidth() - 1;
	Printf("%s",what);
    }
    else
	for (charCount=0; *what; charCount++)
	    Putchar(*what++);
    FlushStdout();
}

Void soFar(t)                          /* Indicate progress towards goal   */
Target t; {                            /* has now reached t                */
    if (quiet
#if EXPLAIN_INSTANCE_RESOLUTION
	      || showInstRes
#endif
			    ) return;
    if (useDots) {
	Int newPos = (Int)((maxPos * ((long)t))/currTarget);

	if (newPos>maxPos)
	    newPos = maxPos;

	if (newPos>currPos) {
	    do
		Putchar('.');
	    while (newPos>++currPos);
	    FlushStdout();
	}
	FlushStdout();
    }
}

Void done() {                          /* Goal has now been achieved       */
    if (quiet
#if EXPLAIN_INSTANCE_RESOLUTION
	      || showInstRes
#endif
			    ) return;
    if (useDots) {
	while (maxPos>currPos++)
	    Putchar('.');
	Putchar('\n');
    }
    else
	for (; charCount>0; charCount--) {
	    Putchar('\b');
#if !(__MWERKS__ && macintosh)
	    Putchar(' ');
	    Putchar('\b');
#endif
	}
    aiming = FALSE;
    FlushStdout();
}

Void failed() {          /* Goal cannot be reached due to    */
    if (aiming) {        /* errors                           */
	aiming = FALSE;
	Putchar('\n');
	FlushStdout();
    }
}
