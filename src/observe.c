/*
 * Debugging via observations.
 *
 * Note: only available via the command interpreter, not
 *       in batch mode.
 *
 */
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "command.h"
#include "errors.h"
#include "machdep.h"
#include "builtin.h"
#include "output.h"
#include "observe.h"

/* --------------------------------------------------------------------------
 * Break dialogue code
 * ------------------------------------------------------------------------*/
#if OBSERVATIONS
static Void   breakDialogue    Args((String));

static struct cmd brkCmds[] =
    { {"p",   BRK_DISPLAY}
    , {"c",   BRK_CONTINUE}
    , {"s",   BRK_SET}
    , {"r",   BRK_RESET}
    , {0,0}
    };

static Void breakDialogue(s)
String s;{
    String arg;
    Int n;
    char cmdstr[80];
    Command cmd;

    normalTerminal();
    do {
	strcpy(cmdstr,"Break @ ");
	promptForInput(strcat(cmdstr,s));
	cmd = readCommand(brkCmds, (Char)0, (Char)'!');
	switch (cmd){
	    case BRK_DISPLAY:	
	       			if ((arg=readFilename())!=0)
				    printObserve(arg);
				else
				    printObserve(ALLTAGS);
	    			break;
	    case BRK_CONTINUE:	
	    			if ((arg=readFilename())!=0){
				    n = atoi(arg);
				    if (n>0) n--;
				    setBreakCount(s,n);
				}
	    			break;
	    case BRK_SET:	if ((arg=readFilename())!=0)
	    			    setBreakpt(arg,TRUE);
				break;
	    case BRK_RESET:	if ((arg=readFilename())==0)
	    			    setBreakpt(s,FALSE);
				else
	    			    setBreakpt(arg,FALSE);
				break;
	}
    } while (cmd!=BRK_CONTINUE);
}
#endif

/* --------------------------------------------------------------------------
 * Observations & breakpoints primops:
 * ------------------------------------------------------------------------*/

#if OBSERVATIONS
#define MAXTAGLENGTH 80
static char obsTag[MAXTAGLENGTH+1];

extPrimFun(primObserve) {		/* the observe primitive for       */
    Cell exp, obsCell;			/* debugging purposes              */
    int i=0;				/*  :: String -> a -> a            */
    fflush(stdout);
    eval(pop());
    while (whnfHead==nameCons) {
	eval(pop());
	if (i<MAXTAGLENGTH) obsTag[i++]=charOf(whnfHead);
	eval(pop());
    }
    obsTag[i]=0;
    				/* create OBSERVE graph marker		   */
    exp  = pop();
    exp  = triple(OBSERVE,exp,0);
    updateRoot(exp);
    /* root is now an INDIRECT node which points to an OBSERVE node 	   */
    /* next create observation list cell for the expression		   */
    obsCell = addObsInstance(obsTag,stack(root),-1);
    /* finally update the OBSERVE node to point to the ons. list cell  	   */
    markedObs(snd(stack(root))) = obsCell;
}

extPrimFun(primBkpt) {			/* check if break enabled          */
    Int i=0;                            /* initiate dialogue               */
    fflush(stdout);
    eval(pop());
    while (whnfHead==nameCons) {
	eval(pop());
	if (i<MAXTAGLENGTH) obsTag[i++]=charOf(whnfHead);
	eval(pop());
    }
    obsTag[i]=0;

    if (breakNow(obsTag)) breakDialogue(obsTag);
    updateRoot(pop());
}

#if !IO_REFS
#error primitive "setBkpt" unavailable as IO_REFS not enabled 
#else
extPrimFun(primSetBkpt) {			
    String s = evalName(IOArg(2));
    eval(IOArg(1));
    checkBool();
    setBreakpt(s, whnfHead == nameTrue);
    IOReturn(nameUnit);
}
#endif

#endif
