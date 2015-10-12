#include "Header.h"
#include <stdio.h>
#include <setjmp.h>
#include "Winhugs.h"

#include "prelude.h"
#include "storage.h"
#include "evaluator.h"
#include "connect.h"
#include "errors.h"
#include "machdep.h"
#include "opts.h"
#include "strutil.h"

// store the extern HINSTANCE
HINSTANCE hThisInstance;

// Command line arguments
CHAR **hugs_argv;
INT	hugs_argc;

/* Construct hugs_argc and hugs_argv from lpszCmdLine */
void copyArgs(LPSTR lpszCmdLine) {

    INT	i, currentArg, beginOfArg;
    CHAR	svChar;

    /* First, get number of args					     */
    /* Rules:							     */
    /* 1) arguments are separates by spaces			     */
    /* 2) A single argument may contain spaces if surrounded by quotes */
    /*								     */
    /* For example, a valid command line with two args is	     */
    /*  c:> winhugs -98 "c:\program files\test.hs"		     */

    hugs_argc = 0;
    for(i=0;lpszCmdLine[i];) {
	if(lpszCmdLine[i]=='"')  { /* a "... " argument */
	    i++;
	    hugs_argc++;
	    while (lpszCmdLine[i] && lpszCmdLine[i] != '"') i++;
	    if (lpszCmdLine[i] != '"') {
		MessageBox(GetFocus(), "Invalid command line", "", MB_OK);
		hugs_argc = 0;
	    }
	}
	else if(lpszCmdLine[i]!=' ') {
	    i++;
	    hugs_argc++;
	    while (lpszCmdLine[i] && lpszCmdLine[i] != ' ')
		i++;
	}

	if(lpszCmdLine[i])
	    i++;
    }

    hugs_argc++; /* One more for program name */

    /* Allocate arguments */
    hugs_argv = malloc(hugs_argc*sizeof(CHAR *));

    /* First argument must be program name */
    hugs_argv[0] = strdup("winhugs.exe");

#define copyCurrentArg {					      \
	svChar = lpszCmdLine[i];				      \
	lpszCmdLine[i] = '\0';				      \
	hugs_argv[currentArg++] = strdup(&lpszCmdLine[beginOfArg]);\
	lpszCmdLine[i] = svChar;				      \
    }

    if (hugs_argc > 1) {
	currentArg = 1;
	for(i=0;lpszCmdLine[i];) {

	    if(lpszCmdLine[i]=='"')  { /* a "... " argument */
		beginOfArg = ++i;
		while (lpszCmdLine[i] != '"')
		    i++;
		copyCurrentArg;
	    } else if(lpszCmdLine[i]!=' ') {
		beginOfArg = i;
		while (lpszCmdLine[i] && lpszCmdLine[i] != ' ')
		    i++;
		copyCurrentArg;
	    }

	    if(lpszCmdLine[i]) i++;
	}
    }
#undef copyCurrentArg
}

int main(int  argc,char *argv[]);

void RunEditor(LPSTR File)
{
    if (File[0] == 0) {
	MessageBox(NULL, "Error: /edit option used with no file", "WinHugs", MB_ICONERROR);
	return;
    }

    // do some of the evaluation stuff from initialise()
    // break before any scripts get loaded
    startEvaluator();
    hugsEdit = strCopy(fromEnv("EDITOR",NULL));
    if (hugsEdit == NULL)
	hugsEdit = WinHugsPickDefaultEditor();
    readOptions("-p\"%s> \" -r$$",FALSE);
    readOptionSettings();

    startEdit(0, File);
}

INT APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmdLine, INT nCmdShow)
{
    int i;

    if (strnicmp(lpszCmdLine, "/edit", 5) == 0) {
	lpszCmdLine += 5;
	while (lpszCmdLine[0] == ' ')
	    lpszCmdLine++;
	if (lpszCmdLine[0] == '\"') {
	    char* s = strchr(++lpszCmdLine, '\"');
	    if (s != NULL)
		s[0] = 0;
	}
	RunEditor(lpszCmdLine);
	return 0;
    }

    InitCommonControls();
    LoadLibrary("RICHED20.DLL");

    // Save application instance
    hThisInstance = hInstance;

    hAccelTable = LoadAccelerators(hThisInstance, "HUGSACCELERATORS");
    if (!ShowMainDialog())
	return 1;

    // Call hugs main function
    copyArgs(lpszCmdLine);
    main(hugs_argc, hugs_argv);

    // Leaving hugs ...
    // hWndMain is already destroyed

    // Free allocated memory for command line
    for (i=0; i<hugs_argc; i++) {
	if (hugs_argv[i])
	    free(hugs_argv[i]);
    }
    free(hugs_argv);

    return 0;
}

/// THINGS REQUIRED TO REMOVE WINHUGS.c

// Application name
LPCSTR appName = "Hugs for Windows";

BOOL InAutoReloadFiles = FALSE;	/* TRUE =>loading files before eval*/
Bool autoLoadFiles     = TRUE; /* TRUE => automatically reloaded modified files */

void ErrorBox(LPCSTR Msg)
{
    MessageBox(hThisWindow, Msg, appName, MB_ICONHAND | MB_OK);
}

void InfoBox(LPCSTR Msg)
{
    MessageBox(hThisWindow, Msg, appName, MB_ICONINFORMATION | MB_OK);
}

void WinHugsExit()
{
    DestroyWindow(hThisWindow);
}

#if 1 //USE_THREADS
void  stopEvaluatorThread();
DWORD evaluatorThreadBody(LPDWORD);

HANDLE  evaluatorThread=NULL;
DWORD   evaluatorThreadId;
BOOL    evaluatorThreadRunning = FALSE;
jmp_buf goToEvaluator;

void WinHugsMessagePump()
{
    MSG msg;
    ExecutionFinished();
    while (GetMessage(&msg, NULL, 0, 0) > 0) {
	if (!TranslateAccelerator(hThisWindow, hAccelTable, &msg)) {
	     TranslateMessage(&msg);
	     DispatchMessage(&msg);
	}
    }
}

void loopInBackground()
{
    MSG msg;

    /* WaitForSingleObject(evaluatorThread, INFINITE); */
    while ( evaluatorThreadRunning && GetMessage(&msg, NULL, 0, 0) ) {
	if (!TranslateAccelerator(hThisWindow, hAccelTable, &msg)) {
	     TranslateMessage(&msg);
	     DispatchMessage(&msg);
	}
    }
    if (evaluatorThreadRunning)
	PostMessage(msg.hwnd, msg.message, msg.wParam, msg.lParam);
}

void stopEvaluatorThread() {

    if(evaluatorThreadRunning){

	if(GetCurrentThreadId() != evaluatorThreadId) {
	    MessageBox(NULL, "stopEvaluatorThread executed by main thread !!!","Error", MB_OK);
	}

	evaluatorThreadRunning = FALSE;
	SuspendThread(evaluatorThread);
	/* stop here until resumed */
	longjmp(goToEvaluator,1);
    }
}

DWORD evaluatorThreadBody(LPDWORD notUsed) {

    int evaluatorNumber = setjmp(goToEvaluator);

#if defined(_MSC_VER) && !defined(_MANAGED)
    /* Under Win32 (when compiled with MSVC), we specially
     * catch and handle SEH stack overflows.
     */
    __try {
#endif

    evaluator(findEvalModule());
    stopEvaluatorThread();

#if defined(_MSC_VER) && !defined(_MANAGED)
    } __except ( ((GetExceptionCode() == EXCEPTION_STACK_OVERFLOW) ?
		  EXCEPTION_EXECUTE_HANDLER :
		  EXCEPTION_CONTINUE_SEARCH) ) {
	/* Closely based on sample code in Nov 1999 Dr GUI MSDN column */
	char* stackPtr;
	static SYSTEM_INFO si;
	static MEMORY_BASIC_INFORMATION mi;
	static DWORD protect;

	/* get at the current stack pointer */
	_asm mov stackPtr, esp;

	/* query for page size + VM info for the allocation chunk we're currently in. */
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

	    /* careful not to do a garbage collection here (as it may have caused the overflow). */
	    WinHugsPutS(stderr, "ERROR - C stack overflow");
	    errFail();
	} else {
	    fatal("C stack overflow; unable to recover.");
	}
    }
#endif
    /* not reached*/
    return 0;
}

void startEvaluatorThread() {
    if (!evaluatorThread) {
	/* Note: I'm assuming that the reason why _beginthreadex() isn't
	 * used is that there's no need to..?
	 */
	evaluatorThread = CreateThread(NULL,
				       0,
				       (LPTHREAD_START_ROUTINE)evaluatorThreadBody,
				       NULL,
				       CREATE_SUSPENDED,
				       &evaluatorThreadId);
    }
    evaluatorThreadRunning = TRUE;
    ResumeThread(evaluatorThread);
}

#endif /* USE_THREADS */
