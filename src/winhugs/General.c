
#include <windows.h>
#include <stdio.h>
#include "Header.h"

#include "machdep.h"
#include "prelude.h"
#include "storage.h"
#include "connect.h"

/* builds a short name for a file path of maximum length MAX_SHORTNAME */
#define MAX_SHORTNAME	40

VOID ShortFileName(CHAR *SrcFileName, CHAR *DestFileName)
{
    CHAR dir[_MAX_PATH], shortDir[_MAX_PATH], shortAux[_MAX_PATH];
    CHAR ext[_MAX_EXT];
    CHAR drive[_MAX_DRIVE];
    CHAR fName[_MAX_FNAME];
    CHAR *ptr;
    BOOL Stop = FALSE;

    /* try to get path */
    _splitpath (SrcFileName, drive, dir, fName, ext);

    /* delete last '\\' char */
    ptr = strrchr(dir,'\\');
    if (ptr)
	*ptr = (CHAR)0;

    wsprintf(shortDir, "\\%s%s", fName, ext);

    while (*dir && !Stop) {
	ptr = strrchr(dir,'\\');

	if(strlen(shortDir)+strlen(ptr) < MAX_SHORTNAME) {
	    /* shortDir = ptr ++ shortDir */
	    sprintf(shortAux, "%s%s", ptr, shortDir);
	    strcpy(shortDir, shortAux);

	    /* delete appended string from dir */
	    *ptr = (CHAR)0;
	} else
	    Stop = TRUE;
    }

    if (*dir)
	wsprintf(DestFileName, "%s\\...%s",drive,shortDir);
    else
	wsprintf(DestFileName, "%s%s",drive,shortDir);

}

/* Call this function in WM_INITDIALOG to center the dialog in Parent window */
// Taken from the MSDN, Using Dialog Boxes
void CenterDialogInParent(HWND hDlg)
{
    RECT rDlg, rMain;
    int posX, posY;

    GetWindowRect(hDlg, &rDlg);
    GetWindowRect(GetParent(hDlg), &rMain);

    posX = rMain.left+((rMain.right-rMain.left-(rDlg.right - rDlg.left))/2);
    posY = rMain.top+((rMain.bottom-rMain.top-(rDlg.bottom - rDlg.top))/2);

    if (posX < 0) posX = 0;
    if (posY < 0) posY = 0;

    SetWindowPos(hDlg, NULL, posX, posY, 0, 0, SWP_NOSIZE | SWP_NOACTIVATE);
}

/* change working directory */
void SetWorkingDir(LPCSTR Src)
{
    CHAR path[_MAX_PATH];
    CHAR drive[_MAX_DRIVE];
    CHAR thePath[2*_MAX_PATH];

    /* ignore file name and extension */
    _splitpath (Src, drive, path, NULL, NULL);
    wsprintf(thePath,"%s%s",drive,path);

    /* Set path */
    SetCurrentDirectory(thePath);
}

// Works for HTML Documents, http links etc
// if it starts with file:, then fire off the filename
void ExecuteFile(LPSTR FileName)
{
    if (strncmp(FileName, "file:", 5) == 0) {
	//find the line number
	char Buffer[MAX_PATH];
	char* s = strrchr(FileName, ':');
	int LineNo;
	if (s != NULL) {
	    int i;
	    s++;
	    for (i = 0; s[i] != 0; i++) {
		if (!isdigit(s[i])) {
		    s = NULL;
		    break;
		}
	    }
	}

	if (s == NULL)
	    LineNo = 0; // the null line
	else {
	    s[-1] = 0;
	    LineNo = atoi(s);
	}

	FileName += 5;			/* skip over "file:" */
	if (strncmp("{Hugs}", FileName, 6) == 0) {
	    strcpy(Buffer, hugsdir());
	    strcat(Buffer, &FileName[6]);
	} else if (FileName[0] == '.' && FileName[1] == '\\') {
	    GetCurrentDirectory(MAX_PATH, Buffer);
	    strcat(Buffer, &FileName[1]);
	} else
	    strcpy(Buffer, FileName);

	startEdit(LineNo, Buffer);
    } else {
	int Res = (int) ShellExecute(hThisWindow, NULL, FileName, NULL, NULL, SW_SHOWNORMAL);
	if (Res <= 32) {
	    char Buffer[MAX_PATH*2];
	    strcpy(Buffer, "Failed to launch file:\n");
	    strcat(Buffer, FileName);
	    MessageBox(hThisWindow, Buffer, "Hugs98", MB_ICONWARNING);
	}
    }
}

// same as ExecuteFile, but relative to the Doc's directory
void ExecuteFileDocs(LPSTR FileName)
{
    char Buffer[MAX_PATH*2];
    char* s;

    GetModuleFileName(hThisInstance, Buffer, MAX_PATH);

    s = strrchr(Buffer, '\\');
    if (s == NULL) s = strrchr(Buffer, '/');
    if (s != NULL) s[1] = 0;

    strcat(Buffer, "docs\\");
    strcat(Buffer, FileName);
    ExecuteFile(Buffer);
}

/* expands characters like \ to \\ in a file name */
LPSTR ExpandFileName(LPSTR what)
{
    static CHAR Expanded[2048];

    if (*what == '\"') {
	strcpy(Expanded, what);
    } else {
	LPSTR where, t, unlex;

	strcpy(Expanded,"\"");

	for(where = &Expanded[1],t=what; *t; ++t) {
	    unlex = unlexChar(*t,'"');
	    wsprintf(where, "%s", unlexChar(*t,'"'));
	    where += strlen(unlex);
	}
	wsprintf(where, "\"%c", '\0');
    }
    return Expanded;
}

BOOL ShowOpenFileDialog(HWND hParent, LPSTR FileName)
{
    OPENFILENAME ofn;

    FileName[0] = 0;
    memset(&ofn, 0, sizeof(ofn));
    ofn.lStructSize = sizeof(OPENFILENAME);
    ofn.hInstance = hThisInstance;
    ofn.hwndOwner = hParent;
    ofn.lpstrFilter = "Haskell Files (*.hs;*.lhs)\0*.hs;*.lhs\0All Files (*.*)\0*.*\0";
    ofn.nFilterIndex = 1;
    ofn.lpstrFile= FileName;
    ofn.nMaxFile = MAX_PATH;
    ofn.lpstrFileTitle = NULL;
    ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY | OFN_EXPLORER;
    ofn.lpfnHook = NULL;
    ofn.lpstrInitialDir = NULL;

    return GetOpenFileName(&ofn);
}
