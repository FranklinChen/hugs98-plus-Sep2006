/*
 * DllMain() for Hugsscript.dll
 */
#include <windows.h>
#define HUGS_SERVER 1
#include "prelude.h"
#include "storage.h"
#include "connect.h"
/* included to provide stub defn of some unsupported primops. */
#include "observe.h"

extern void setHugsModule(HMODULE);

BOOL
WINAPI
DllMain(
  HINSTANCE hinstDLL,  /* handle to the DLL module    */
  DWORD fdwReason,     /* reason for calling function */
  LPVOID lpvReserved   /* reserved                    */
)
{
    switch(fdwReason) {
    case DLL_PROCESS_ATTACH:
	/* Stash away the HMODULE for later use. */
	setHugsModule(hinstDLL);
	break;
    }
    return TRUE;
}

