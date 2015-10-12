/*
    Editors - detect installed editors and their command line
    Used by the options dialog
*/

#include "Header.h"
#include <stdio.h>

// HELPER

BOOL RegGet(HKEY Key, LPCTSTR Subkey, LPTSTR Buffer)
{
    LONG Size = MAX_PATH;
    return (RegQueryValue(Key, Subkey, Buffer, &Size) == ERROR_SUCCESS);
}

// CORE

BOOL CalcNotepad(LPTSTR Buffer)
{
    GetWindowsDirectory(Buffer, MAX_PATH);
    strcat(Buffer, "\\notepad.exe");
    return TRUE;
}

BOOL CalcTextpad(LPTSTR Buffer)
{
    //[HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\TextPad.exe]
    //@="D:\\Program Files\\TextPad 4\\TextPad.exe"

    char* s;
    if (!RegGet(HKEY_LOCAL_MACHINE,
	"SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\TextPad.exe",
	Buffer)) return FALSE;

    s = strrchr(Buffer, '\\');
    if (s == NULL) return FALSE;
    strcpy(&s[1], "System\\DDEOPN32.EXE TextPad %s(%d)");
    return TRUE;
}

BOOL CalcVim(LPTSTR Buffer)
{
    // x = HKEY_LOCAL_MACHINE\SOFTWARE\Classes\Vim.Application\CLSID
    // vi = HKEY_LOCAL_MACHINE\SOFTWARE\Classes\CLSID\$x$\LocalServer32
    // cmd = "$vi$" +%d %s
    char Buf2[1000];

    if (!RegGet(HKEY_LOCAL_MACHINE,
	"SOFTWARE\\Classes\\Vim.Application\\CLSID",
	Buffer)) return FALSE;

    sprintf(Buf2, "SOFTWARE\\Classes\\CLSID\\%s\\LocalServer32", Buffer);
    if (!RegGet(HKEY_LOCAL_MACHINE, Buf2, &Buffer[1]))
	return FALSE;

    Buffer[0] = '\"';
    strcat(Buffer, "\" --remote-silent +%d %s");
    return TRUE;
}

BOOL CalcSyn(LPTSTR Buffer)
{
    Buffer[0] = '\"';
    if (!RegGet(HKEY_CURRENT_USER,
        "Software\\Ascher\\Syn", &Buffer[1])) return FALSE;
    strcat(Buffer, "\" -line %d \"%s\"");
    return TRUE;
}

// DRIVER
// A list of the editors
struct {
    LPCTSTR Name;
    BOOL (*Func)(LPSTR Buffer);
} const Editors[] = {
    {"Notepad", CalcNotepad},
    {"Textpad (www.textpad.com)", CalcTextpad},
    {"Vim (www.vim.org)", CalcVim},
    {"Syn (syn.sourceforge.net)", CalcSyn},
    {NULL, NULL}
};

// Index is 0 based, which editor to return
// Buffer is the result, the path to the editor.
// Buffer[0] = 0 means this editor is not installed
// Return is the name of the editor.
// Return = NULL implies no more editors
LPCTSTR GetEditor(int Index, LPTSTR Buffer)
{
    Buffer[0] = 0;
    if (Editors[Index].Name == NULL)
        return NULL;

    if (!Editors[Index].Func(Buffer))
        Buffer[0] = 0;
    return Editors[Index].Name;
}

char* WinHugsPickDefaultEditor()
{
    TCHAR Buffer[MAX_PATH];
    int i;
    for (i = 1; GetEditor(i, Buffer) != NULL; i++) {
	if (Buffer[0] != 0)
	    return strdup(Buffer);
    }
    GetEditor(0, Buffer);
    if (Buffer[0] == 0)
	return NULL;
    else
	return strdup(Buffer);
}
