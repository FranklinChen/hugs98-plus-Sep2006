#include "Header.h"
#include <stdio.h>
#include "Winhugs.h"

#include <prelude.h>
#include <storage.h>
#include <connect.h>

// stdstr output definitions
#define MAX_STDSTR 1024
int     StrInx = 0;
FILE   *stdstr = NULL;
char    stdstrbuff[MAX_STDSTR];


// beacuse i need to use it
// otherwise you call yourself
#undef fputc

void WinHugsPutText(FILE* f, char* Buffer, BOOL Char)
{
    char First = Buffer[0];
    if (Char && First == 0)
	Buffer[0] = '\1';

    if (f == stderr) {
	int LastColor = WinHugsColor(RED);
	RtfWindowPutS(Buffer);
	WinHugsColor(LastColor);
    } else if (f == stdout) {
	RtfWindowPutS(Buffer);
    } else if (f == stdstr) {
	int i;
	for (i = 0; Buffer[i]; i++) {
	    if (Buffer[i] == '\n') {
		stdstrbuff[StrInx] = 0;
		StrInx = 0;
	    }
	    else
		stdstrbuff[StrInx++] = Buffer[i];
	}
    } else {
	if (Char)
	    fputc(First, f);
	else
	    fputs(Buffer, f);
    }
}

void WinHugsPutS(FILE* f, char* Buffer)
{
    WinHugsPutText(f, Buffer, FALSE);
}

int WinHugsAnyPrintf(FILE* f, const char* format, va_list* args)
{
    char Buffer[2048];
    int Count = vsprintf(Buffer, format, *args);
    WinHugsPutS(f, Buffer);
    return Count;
}

int WinHugsPrintf(const char* format, ...)
{
    va_list args;
    int Count;
    va_start(args, format);
    Count = WinHugsAnyPrintf(stdout, format, &args);
    va_end(args);
    return Count;
}

int WinHugsFPrintf(FILE* f, const char* format, ...)
{
    va_list args;
    int Count;
    va_start(args, format);
    Count = WinHugsAnyPrintf(f, format, &args);
    va_end(args);
    return Count;
}

// Must be an int and not a char
// Otherwise -1 return codes get created, and hugs exits
int WinHugsPutC(FILE* f, int c)
{
    char Buf[2];
    Buf[0] = (char) c;
    Buf[1] = 0;
    WinHugsPutText(f, Buf, TRUE);
    return c;
}

/////////////////////////////////////////////////////////////////////
// IMPLEMENT getChar and interact

BOOL ValidMutexes = FALSE;
CRITICAL_SECTION Mutex;
HANDLE Contents;
#define KeyboardBufferSize 25
CHAR KeyboardBuffer[KeyboardBufferSize];
int KeyboardBufferCount = 0;

void EnterContents()
{
    WaitForSingleObject(Contents, INFINITE);
}

void ExitContents()
{
    ReleaseSemaphore(Contents, 1, NULL);
}

void IORemapBegin()
{
    // Put the mutexes in a sane state
    // Kill then create them
    if (ValidMutexes) {
	DeleteCriticalSection(&Mutex);
	CloseHandle(Contents);
    }
    InitializeCriticalSection(&Mutex);
    Contents = CreateSemaphore(NULL, 0, 1, NULL);
    ValidMutexes = TRUE;
}

void IORemapEnd()
{
    // Send a dead char, to wake up the semaphore if locked
    WinHugsReceiveC(0);
}

// Called when a character gets sent to WinHugs while it is running
void WinHugsReceiveC(int c)
{
    EnterCriticalSection(&Mutex);
    if (KeyboardBufferCount != KeyboardBufferSize) {
        KeyboardBuffer[KeyboardBufferCount] = c;
	KeyboardBufferCount++;
	if (KeyboardBufferCount == 1)
	    ExitContents();
    }
    LeaveCriticalSection(&Mutex);
}

int WinHugsGetC(FILE* f)
{
    if (f == stdin) {
        int Res, i;

	EnterCriticalSection(&Mutex);
	if (KeyboardBufferCount == 0) {
            SetStatusBar("Waiting for user input");
	    LeaveCriticalSection(&Mutex);
	    EnterContents();
	    EnterCriticalSection(&Mutex);
	    SetStatusBar("");
	}

	Res = KeyboardBuffer[0];
	for (i = 1; i < KeyboardBufferSize; i++)
	    KeyboardBuffer[i-1] = KeyboardBuffer[i];
	KeyboardBufferCount--;

	if (KeyboardBufferCount > 0)
	    ExitContents();
	LeaveCriticalSection(&Mutex);

	// fix problem with char/int truncation
	if (Res < 0)
	    Res += 256;

	WinHugsPutC(stdout, Res);
	return Res; // no support for interact
    } else
	return fgetc(f);
}

void WinHugsFilename(const char* FileName, int LineNo)
{
    LPCTSTR HugsDir = hugsdir();
    int nHugsDir = strlen(HugsDir);
    int nCurDir;
    char Buffer[MAX_PATH], CurDir[MAX_PATH];
    nCurDir = GetCurrentDirectory(MAX_PATH, CurDir);

    strcpy(Buffer, "file:");

    if (strncmp(HugsDir, FileName, nHugsDir) == 0) {
	strcat(Buffer, "{Hugs}");
	strcat(Buffer, FileName + nHugsDir);
    } else if (strnicmp(CurDir, FileName, nCurDir) == 0) {
	strcat(Buffer, ".");
	strcat(Buffer, FileName + nCurDir);
    } else
	strcat(Buffer, FileName);

    if (LineNo) {
	int n = strlen(Buffer);
	Buffer[n] = ':';
	itoa(LineNo, Buffer + n+1, 10);
    }

    WinHugsHyperlink(Buffer);
}
