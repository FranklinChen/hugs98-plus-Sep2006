#include "Header.h"

#include "prelude.h"
#include "resrc1.h"
#include "opts.h"
#include "storage.h"
#include "machdep.h"

INT_PTR CALLBACK OptionsHugsProc(HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam);
INT_PTR CALLBACK OptionsRuntimeProc(HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam);
INT_PTR CALLBACK OptionsCompileProc(HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam);

BOOL ShowOptionsDialog(HWND hParent)
{
    const int nPages = 3;
    PROPSHEETPAGE psp[3];
    PROPSHEETHEADER psh;
    int i;

    for (i = 0; i < nPages; i++) {
	psp[i].dwSize = sizeof(psp[i]);
	psp[i].dwFlags = PSP_DEFAULT;
	psp[i].hInstance = hThisInstance;
    }

    psp[0].pszTemplate = MAKEINTRESOURCE(DLG_OPTCOMPILE);
    psp[0].pfnDlgProc = OptionsCompileProc;

    psp[1].pszTemplate = MAKEINTRESOURCE(DLG_OPTRUNTIME);
    psp[1].pfnDlgProc = OptionsRuntimeProc;

    psp[2].pszTemplate = MAKEINTRESOURCE(DLG_OPTHUGS);
    psp[2].pfnDlgProc = OptionsHugsProc;

    psh.dwSize = sizeof(psh);
    psh.dwFlags = PSH_NOAPPLYNOW | PSH_PROPSHEETPAGE;
    psh.hwndParent = hParent;
    psh.hInstance = hThisInstance;
    psh.pszCaption = "Hugs Options";
    psh.nPages = nPages;
    psh.nStartPage = 0;
    psh.ppsp = &psp[0];
    PropertySheet(&psh);

    // Might have changed, no way to know...
    return TRUE;
}

BOOL GetDlgItemBool(HWND hDlg, INT CtrlID)
{
    return (IsDlgButtonChecked(hDlg, CtrlID) == BST_CHECKED);
}

void SetDlgItemBool(HWND hDlg, INT CtrlID, BOOL Value)
{
    CheckDlgButton(hDlg, CtrlID, Value ? BST_CHECKED : BST_UNCHECKED);
}

/////////////////////////////////////////////////////////////////////
// OPTHUGS related code

int CALLBACK ListAllFonts(CONST LOGFONT* lpelfe, CONST TEXTMETRIC* lpntme, DWORD FontType, LPARAM lParam)
{
    HWND hLst = (HWND) lParam;
    LPCTSTR FontName = (LPCTSTR) lpelfe->lfFaceName;
    if (SendMessage(hLst, CB_FINDSTRINGEXACT, -1, (LPARAM) FontName) == CB_ERR)
	SendMessage(hLst, CB_ADDSTRING, 0, (LPARAM) FontName);
    return 1;
}

int TwipToPoint(x){return x / 20;}
int PointToTwip(x){return x * 20;}

void CalculateFont(HWND hDlg, CHARFORMAT* cf)
{
    BOOL ValidSize;
    int NewSize;
    int CurSel;
    HWND hFace = GetDlgItem(hDlg, lstFontFace);

    RegistryReadFont(cf);

    CurSel = (int) SendMessage(hFace, CB_GETCURSEL, 0, 0);
    if (CurSel == CB_ERR)
	GetWindowText(hFace, cf->szFaceName, 32);
    else
	SendMessage(hFace, CB_GETLBTEXT, CurSel, (LPARAM) cf->szFaceName);

    cf->dwEffects = 0;
    cf->dwEffects |= (GetDlgItemBool(hDlg, chkFontBold) ? CFE_BOLD : 0);
    cf->dwEffects |= (GetDlgItemBool(hDlg, chkFontItalic) ? CFE_ITALIC : 0);

    // check the size
    NewSize = GetDlgItemInt(hDlg, txtFontSize, &ValidSize, FALSE);
    if (ValidSize) cf->yHeight = PointToTwip(NewSize);
}

void UpdateFontPreview(HWND hDlg)
{
    CHARFORMAT cf;
    HWND hRTF = GetDlgItem(hDlg, rtfPreview);

    CalculateFont(hDlg, &cf);
    SendMessage(hRTF, EM_SETCHARFORMAT, SCF_ALL, (LPARAM) &cf);
}

void InitOptionsFont(HWND hDlg)
{
    // load up the list of fonts
    HDC hDC = GetDC(hDlg);
    CHARFORMAT cf;
    LOGFONT lf;

    SendDlgItemMessage(hDlg, spnFontSize, UDM_SETRANGE, 0, MAKELONG(72, 6));

    lf.lfCharSet = DEFAULT_CHARSET;
    lf.lfFaceName[0] = 0;
    lf.lfPitchAndFamily = 0;
    EnumFontFamiliesEx(hDC, &lf, ListAllFonts, (LPARAM) GetDlgItem(hDlg, lstFontFace), 0);
    ReleaseDC(hDlg, hDC);

    SetDlgItemText(hDlg, rtfPreview, "Text Preview ABC abc 123");

    // setup the config options
    RegistryReadFont(&cf);
    SetDlgItemText(hDlg, lstFontFace, cf.szFaceName);
    SetDlgItemBool(hDlg, chkFontBold, cf.dwEffects & CFE_BOLD);
    SetDlgItemBool(hDlg, chkFontItalic, cf.dwEffects & CFE_ITALIC);
    SetDlgItemInt(hDlg, txtFontSize, TwipToPoint(cf.yHeight), FALSE);

    UpdateFontPreview(hDlg);
}

void InitOptionsEditor(HWND hDlg)
{
    HWND hLst = GetDlgItem(hDlg, lstEditor);

    char* s;
    char Buffer[MAX_PATH];
    int i;

    // Deal with the async flag
    s = hugsEdit;
    while (*s == '&')
	s++;

    // now set the text to the remaining commmand
    // and figure out if it corresponds to any defaults
    SetDlgItemText(hDlg, txtEditor, s);

    for (i = 0; ; i++) {
        LPCTSTR EditorName = GetEditor(i, Buffer);
	if (EditorName == NULL) break;
	if (Buffer[0]) {
	    int NewIndex = (int) SendMessage(hLst, CB_ADDSTRING, 0, (LPARAM) EditorName);
	    SendMessage(hLst, CB_SETITEMDATA, NewIndex, i);
	    if (stricmp(Buffer, s) == 0)
		SendMessage(hLst, CB_SETCURSEL, NewIndex, 0);
	}
    }

    i = (int) SendMessage(hLst, CB_ADDSTRING, 0, (LPARAM) "Custom...");
    SendMessage(hLst, CB_SETITEMDATA, i, -1);
    if (SendMessage(hLst, CB_GETCURSEL, 0, 0) == -1) {
	SendMessage(hLst, CB_SETCURSEL, i, 0);
	EnableWindow(GetDlgItem(hDlg, txtEditor), TRUE);
    }
}

void WriteOptions()
{
    writeRegString("Options", optionsToStr());
}

INT_PTR CALLBACK OptionsHugsProc(HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam)
{
    switch (msg) {
    case WM_INITDIALOG:
	InitOptionsFont(hDlg);
	InitOptionsEditor(hDlg);
	break;

    case WM_COMMAND:
	{
	    int Code = HIWORD(wParam);
	    int Ctrl = LOWORD(wParam);

	    if ((Ctrl == chkFontBold && Code == BN_CLICKED) ||
		(Ctrl == chkFontItalic && Code == BN_CLICKED) ||
		(Ctrl == txtFontSize && Code == EN_CHANGE) ||
		(Ctrl == lstFontFace && (Code == CBN_EDITCHANGE || Code == CBN_SELCHANGE))
	       )
		UpdateFontPreview(hDlg);

	    if (Ctrl == lstEditor && Code == CBN_SELCHANGE) {
		HWND hLst = GetDlgItem(hDlg, lstEditor);
		HWND hTxt = GetDlgItem(hDlg, txtEditor);
		int CurSel = (int) SendMessage(hLst, CB_GETCURSEL, 0, 0);
		int CurData = (int) SendMessage(hLst, CB_GETITEMDATA, CurSel, 0);
		EnableWindow(hTxt, CurData == -1);

		if (CurData != -1) {
		    CHAR Buffer[MAX_PATH];
		    GetEditor(CurData, Buffer);
		    SetWindowText(hTxt, Buffer);
		}
	    }
	}
	break;

    case WM_NOTIFY:
	if (((NMHDR*) lParam)->code == PSN_APPLY) {
	    CHARFORMAT cf;
	    char Buffer[MAX_PATH];

	    CalculateFont(hDlg, &cf);
	    RegistryWriteFont(&cf);

	    // do the path stuff
	    Buffer[0] = '&';
	    GetDlgItemText(hDlg, txtEditor, &Buffer[1], MAX_PATH);

	    free(hugsEdit);
	    hugsEdit = strdup(Buffer);

	    WriteOptions();
	}
	break;
    }
    return (INT_PTR)FALSE;
}

/////////////////////////////////////////////////////////////////////
// OPTRUNTIME related code
#define MIN_HEAP_SIZE 1
#define MAX_HEAP_SIZE 1000

int Heap2Mb(int heap){return max(1, heap * 8 / (1024 * 1024));}
int Mb2Heap(int mb){return (mb * 1024 * 1024) / 8;}

INT_PTR CALLBACK OptionsRuntimeProc(HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam)
{
    switch (msg) {
	case WM_INITDIALOG:
	    // heapSize is 8 byte cells
	    SetDlgItemInt(hDlg, txtHeapSize, Heap2Mb(hpSize), FALSE);
	    SendDlgItemMessage(hDlg, spnHeapSize, UDM_SETRANGE, 0, MAKELONG(MAX_HEAP_SIZE,MIN_HEAP_SIZE));

	    SetDlgItemBool(hDlg, chkUserShow, useShow);
	    SetDlgItemBool(hDlg, chkPrintStats, showStats);
	    SetDlgItemBool(hDlg, chkPrintType, addType);
	    SetDlgItemBool(hDlg, chkPrintGC, gcMessages);
	    break;

	case WM_NOTIFY:
	    if (((NMHDR*) lParam)->code == PSN_APPLY) {
		// apply here
	        int size = GetDlgItemInt(hDlg, txtHeapSize, NULL, FALSE);
		if (MIN_HEAP_SIZE <= size && size <= MAX_HEAP_SIZE)
		    hpSize = Mb2Heap(size);

		useShow = GetDlgItemBool(hDlg, chkUserShow);
		showStats = GetDlgItemBool(hDlg, chkPrintStats);
		addType = GetDlgItemBool(hDlg, chkPrintType);
		gcMessages = GetDlgItemBool(hDlg, chkPrintGC);

		WriteOptions();
	    }
	    break;
    }
    return (INT_PTR)FALSE;
}

/////////////////////////////////////////////////////////////////////
// OPTCOMPILE related code

void EnableHaskellExts(HWND hDlg)
{
    BOOL Ext = GetDlgItemBool(hDlg, optExtensions);

    EnableWindow(GetDlgItem(hDlg, chkOverlap), Ext);
    EnableWindow(GetDlgItem(hDlg, chkHereDocs), Ext);
    EnableWindow(GetDlgItem(hDlg, chkOverlapUnsafe),
	Ext && GetDlgItemBool(hDlg, chkOverlap));
}

INT_PTR CALLBACK OptionsCompileProc(HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam)
{
    switch (msg) {
	case WM_INITDIALOG:
	    SetDlgItemText(hDlg, txtPath, hugsPath);
	    SetDlgItemBool(hDlg, chkListLoading, listScripts);
	    SetDlgItemBool(hDlg, chkAutoReload, autoLoadFiles);

	    SetDlgItemBool(hDlg, optCompatible, haskell98);
	    SetDlgItemBool(hDlg, optExtensions, !haskell98);
	    SetDlgItemBool(hDlg, chkOverlap, allowOverlap);
	    SetDlgItemBool(hDlg, chkOverlapUnsafe, allowUnsafeOverlap);
	    SetDlgItemBool(hDlg, chkHereDocs, hereDocs);
	    EnableHaskellExts(hDlg);
	    break;

	case WM_NOTIFY:
	    if (((NMHDR*) lParam)->code == PSN_APPLY) {
		// apply here
		int n = GetWindowTextLength(GetDlgItem(hDlg, txtPath));
		free(hugsPath);
		hugsPath = malloc(n+5);
		GetDlgItemText(hDlg, txtPath, hugsPath, n+3);

		listScripts = GetDlgItemBool(hDlg, chkListLoading);
		autoLoadFiles = GetDlgItemBool(hDlg, chkAutoReload);
		haskell98 = GetDlgItemBool(hDlg, optCompatible);
		allowOverlap = GetDlgItemBool(hDlg, chkOverlap);
		allowUnsafeOverlap = GetDlgItemBool(hDlg, chkOverlapUnsafe);
		hereDocs = GetDlgItemBool(hDlg, chkHereDocs);

		WriteOptions();
	    }
	    break;

	case WM_COMMAND:
	    if (LOWORD(wParam) == chkOverlap ||
		LOWORD(wParam) == optExtensions ||
		LOWORD(wParam) == optCompatible)
		EnableHaskellExts(hDlg);
	    break;
    }
    return (INT_PTR)FALSE;
}
