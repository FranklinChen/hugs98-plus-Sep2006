#include "prelude.h"
#include "Header.h"
#include "resrc1.h"

#define MruCount  9

// run over by one, so registry code can see the end
// always MruBuffer[MruCount] == NULL
char* MruBuffer[MruCount+1] = {0};
int MenusShown = 1; // the default one

LPSTR MruGetItem(int i)
{
    return MruBuffer[i];
}

void ShowMruMenu()
{
    HMENU hMenu = GetSubMenu(GetMenu(hThisWindow), 0);

    int n, i;
    //first count the MRU list
    for (n = 0; MruBuffer[n] != NULL; n++)
	; // no code required

    //add enough entries
    for (i = MenusShown; i < n; i++)
	AppendMenu(hMenu, MF_STRING, ID_MRU+i, MruBuffer[i]);
    MenusShown = (n == 0 ? 1 : n);

    //then change them
    for (i = 0; i < n; i++)
	ModifyMenu(hMenu, ID_MRU+i, MF_BYCOMMAND, ID_MRU+i, MruBuffer[i]);
}

void MruInit()
{
    RegistryReadMru(MruBuffer);
    ShowMruMenu();
}

// Hook into WinHugs for Hugs
void WinHugsAddMruFile(const char* file)
{
    // if its already in the list move it to the top
    // if its not, add it at the top
    int i;
    BOOL Found;

    // remove from the list if its already there
    Found = FALSE;
    for (i = 0; MruBuffer[i] != NULL; i++) {
	Found = Found || (stricmp(MruBuffer[i], file) == 0);
	if (Found)
	    MruBuffer[i] = MruBuffer[i+1]; //rely on trailing NULL
    }

    // if the last entry would die, kill it now
    if (MruBuffer[MruCount-1] != NULL)
	free(MruBuffer[MruCount-1]);

    // shift everything along by one
    for (i = MruCount-1; i > 0; i--)
	MruBuffer[i] = MruBuffer[i-1];

    // and put the new file at the top
    MruBuffer[0] = strdup(file);

    RegistryWriteMru(MruBuffer);
    ShowMruMenu();
}
