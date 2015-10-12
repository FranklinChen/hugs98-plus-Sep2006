/* --------------------------------------------------------------------------
 * DlgMain.c: José Enrique Gallardo Ruiz, Feb 1999
 *            Neil Mitchell, 2005
 *
 * This file contains the implementation for a frame window definition
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "resrc1.h"
#include "Header.h"
#include "Winmenu.h"

HWND hThisWindow;
HACCEL hAccelTable;

//copied, most of these are redundant
#include <windows.h>
#include <signal.h>
#include <richedit.h>

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"

#include "Registry.h"
#include "RtfWindow.h"

// Is the Interpretter currently going wirrr...
BOOL Running = FALSE;

INT_PTR CALLBACK MainDlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

// ID's for the items that are not in the resource dialog
const int ID_STATUS = 9500;
const int ID_TOOLBAR = 9600;

void EnableButtons();

int Buttons[] = {
    // -1 is a separator, 0 is the end
    ID_OPEN, -1,
    ID_CUT, ID_COPY, ID_PASTE, -1,
    ID_RUN, ID_STOP, ID_MAKE, ID_SETOPTIONS, -1,
    ID_HELPCONTENTS,
    0
};

BOOL ShowMainDialog()
{
    HWND hWnd = CreateDialog(hThisInstance, MAKEINTRESOURCE(DLG_MAIN), NULL, &MainDlgProc);

    if (hWnd == NULL)
    {
	MessageBox(NULL, "Failed to create main WinHugs dialog", "WinHugs", MB_ICONERROR);
	return FALSE;
    }
    return TRUE;
}

void MainInitToolbar(HWND hWnd)
{
    int i;
    int AnyButtons = 0, RealButtons = 0;
    TBBUTTON* TbButtons;
    HWND hToolbar;
    HIMAGELIST hImgList;
    HBITMAP hBmp;

    for (AnyButtons = 0; Buttons[AnyButtons] != 0; AnyButtons++)
	; // no code required

    TbButtons = malloc(sizeof(TBBUTTON) * AnyButtons);
    for (i = 0; i < AnyButtons; i++) {
	if (Buttons[i] == -1) {
	    TbButtons[i].iBitmap = 0;
	    TbButtons[i].fsStyle = BTNS_SEP;
	    TbButtons[i].idCommand = 0;
	} else {
	    TbButtons[i].iBitmap = RealButtons;
	    RealButtons++;
	    TbButtons[i].idCommand = Buttons[i];
	    TbButtons[i].fsStyle = TBSTYLE_BUTTON;
	}
	TbButtons[i].fsState = TBSTATE_ENABLED;
	TbButtons[i].dwData = (DWORD_PTR) NULL;
	TbButtons[i].iString = (INT_PTR) NULL;
    }

    hToolbar = CreateWindowEx(
	0,
	TOOLBARCLASSNAME, NULL,
	TBSTYLE_TOOLTIPS | WS_CHILD | WS_VISIBLE | CCS_NODIVIDER | TBSTYLE_FLAT,
	// TBSTYLE_TOOLTIPS | WS_CHILD | WS_VISIBLE | TBSTYLE_WRAPABLE | /*CCS_NORESIZE |*/ CCS_NODIVIDER | TBSTYLE_FLAT,
	0, 0, 600, 28,
	hWnd, (HMENU) ID_TOOLBAR, hThisInstance, NULL);

    // create the image list
    hImgList = ImageList_Create(18, 18, ILC_COLOR4 | ILC_MASK, RealButtons, RealButtons);
    hBmp = LoadBitmap(hThisInstance, MAKEINTRESOURCE(BMP_TOOLBAR));
    ImageList_AddMasked(hImgList, hBmp, RGB(255,0,255));
    DeleteObject(hBmp);
    SendMessage(hToolbar, TB_SETIMAGELIST, 0, (LPARAM) hImgList);

    // setup the toolbar properties
    SendMessage(hToolbar, TB_SETBUTTONSIZE, 0, MAKELONG(24,24));
    SendMessage(hToolbar, TB_SETBITMAPSIZE, 0, MAKELONG(18,18));
    SendMessage(hToolbar, TB_ADDBUTTONS, AnyButtons, (LPARAM) TbButtons);
    free(TbButtons);
}

void MainInitDialog(HWND hWnd)
{
    // so external functions can reference it
    hThisWindow = hWnd;

    // Setup the icons
    SendMessage(hWnd, WM_SETICON, ICON_SMALL,
	(LPARAM) LoadImage(hThisInstance, "HUGS", IMAGE_ICON, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CXSMICON), 0));
    SendMessage(hWnd, WM_SETICON, ICON_BIG,
	(LPARAM) LoadIcon(hThisInstance, "HUGS"));

    // Create the toolbar
    MainInitToolbar(hWnd);

    // Create the status bar
    CreateStatusWindow(WS_CHILD | WS_VISIBLE, "", hWnd, ID_STATUS);

    // Call the required init functions
    RtfWindowInit(GetDlgItem(hWnd, ID_RTF));
    MruInit();
    RegistryReadWindowPos(hWnd);
}

char CommandDelayBuffer[1000];

void FireCommandDelay(LPCSTR Command)
{
    strcpy(CommandDelayBuffer, Command);
    PostMessage(hThisWindow, WM_APP, 0, 0);
}

void FireCommand(LPCSTR Command);

void EnableButton(int id, BOOL Enable)
{
    TBBUTTONINFO tbi;
    tbi.cbSize = sizeof(tbi);
    tbi.dwMask = TBIF_STATE;
    tbi.fsState = (Enable ? TBSTATE_ENABLED : 0);
    SendDlgItemMessage(hThisWindow, ID_TOOLBAR, TB_SETBUTTONINFO, id, (LPARAM) &tbi);

    EnableMenuItem(GetMenu(hThisWindow), id,
	    MF_BYCOMMAND | (Enable ? MF_ENABLED : MF_GRAYED));
}

void EnableButtons()
{
    int CopyState = (Running ? 0 : RtfWindowCanCutCopy());

    EnableButton(ID_STOP, Running);
    EnableButton(ID_RUN, !Running);

    EnableButton(ID_CUT, CopyState & DROPEFFECT_MOVE);
    EnableButton(ID_DELETE, CopyState & DROPEFFECT_MOVE);
    EnableButton(ID_COPY, CopyState & DROPEFFECT_COPY);
    EnableButton(ID_PASTE, !Running);
    EnableButton(ID_CLEARSCREEN, !Running);
    EnableButton(ID_SELECTALL, !Running);
}

void ExecutionFinished()
{
    RtfWindowFlushBuffer();
    RtfWindowStartInput();
    Running = FALSE;
    EnableButtons();
}

void FireCommand(LPCSTR Command)
{
    RtfWindowSetCommand(Command);
    RtfWindowStartOutput();
    AddHistory(Command);

    Running = TRUE;
    EnableButtons();

    stringInput((LPSTR) Command);
    input(BREAK);
    IORemapBegin();
    if (doCommand())
	SendMessage(hThisWindow, WM_CLOSE, 0, 0);

    longjmp(catch_error, 1);
}

void MainSize(HWND hWnd, int x, int y)
{
    RECT rc;
    HWND hStatus = GetDlgItem(hWnd, ID_STATUS);
    HWND hToolbar = GetDlgItem(hWnd, ID_TOOLBAR);
    HWND hRTF = GetDlgItem(hWnd, ID_RTF);
    int HeightStatus, HeightToolbar;

    GetClientRect(hStatus, &rc);
    MoveWindow(hStatus, 0, y - rc.bottom, x, rc.bottom, TRUE);
    HeightStatus = rc.bottom;

    GetClientRect(hToolbar, &rc);
    HeightToolbar = rc.bottom;

    // hack-o-rama
    if (HeightToolbar == 26)
	HeightToolbar = 28;

    MoveWindow(hRTF, 0, HeightToolbar, x, y - HeightToolbar - HeightStatus, FALSE);
}

//#define DlgSendMessage(h,c,w,l)  SendMessage((h),(c),MAKEWPARAM(w,(HIWORD(l))),(LOWORD(l)))
//#define AbortInterpreter	 input(BREAK); WinPuts(hWndText, "\n")
//#define GotoInterpreter 	 longjmp(catch_error, 1);

void MainOpenFile(HWND hWnd)
{
    CHAR FileName[MAX_PATH];
    CHAR Command[2048];

    if (ShowOpenFileDialog(hWnd, FileName)) {
	wsprintf(Command, ":load %s", ExpandFileName((String)FileName));
	FireCommand(Command);
    }
}

void AbortExecution()
{
    raise(SIGINT);
    IORemapEnd();
}

void MainCommand(HWND hWnd, int ID)
{
    switch (ID) {
	case IDCANCEL: EndDialog(hWnd, 0); break;
	case ID_OPEN: MainOpenFile(hWnd); break;
	case ID_SCRIPTMAN: ShowScriptMan(); break;
	case ID_EXIT: FireCommand(":quit\n"); break;

	/* Load one of the last 10 open files */
	case ID_MRU+0: case ID_MRU+1: case ID_MRU+2: case ID_MRU+3:
	case ID_MRU+4: case ID_MRU+5: case ID_MRU+6: case ID_MRU+7:
	case ID_MRU+8: case ID_MRU+9:
	    {
		char Command[1000];
		wsprintf(Command, ":load %s", ExpandFileName(MruGetItem(ID-ID_MRU)));
		FireCommand(Command);
	    }
	    break;

	// EDIT MENU
	case ID_CUT: RtfWindowClipboard(WM_CUT); break;
	case ID_COPY: RtfWindowClipboard(WM_COPY); break;
	case ID_PASTE: RtfWindowClipboard(WM_PASTE); break;
	case ID_CLEARSCREEN: RtfWindowClear(); break;
	case ID_DELETE: RtfWindowDelete(); break;
	case ID_SELECTALL: RtfWindowSelectAll(); break;
	case ID_GOPREVIOUS: RtfWindowHistory(-1); break;
	case ID_GONEXT: RtfWindowHistory(+1); break;


	// ACTIONS MENU
	// Reload script files
	case ID_COMPILE: case ID_MAKE: FireCommand(":reload"); break;
	case ID_CLEARALL: FireCommand(":load"); break;
	case ID_GOEDIT: FireCommand(":edit"); break;

	/* Stop program execution */
	case ID_STOP:
	    MessageBeep(0xFFFFFFFF);
	    AbortExecution();
	    break;

	/* Evaluate main expression */
	case ID_RUN:
	    {
		char Buffer[1000];
		RtfWindowGetCommand(Buffer);
		if (Buffer[0] == '\0')
			FireCommand(":main");
		else
		    FireCommand(Buffer);
	    }
	    break;

	/* Set interpreter options using dialog box */
	case ID_SETOPTIONS:
	    if (ShowOptionsDialog(hWnd))
		RtfWindowUpdateFont();
	    break;


	// BROWSE MENU
	case ID_BROWSEHIERARCHY: DrawClassesHierarchy(); break;
	case ID_BROWSECLASSES: DoBrowseClasses(); break;
	case ID_BROWSENAMES: DoBrowseNames(); break;
	case ID_BROWSETYCONS: DoBrowseTycons(); break;

	// HELP MENU
	case ID_HELPCONTENTS: ExecuteFileDocs("hugs98.chm"); break;
	case ID_HELPCOMMANDS: FireCommand(":?\n"); break;
	case ID_LIBRARIES: ExecuteFile("http://www.haskell.org/ghc/docs/latest/html/libraries/index.html"); break;
	case ID_WWWHASKELL: ExecuteFile("http://haskell.org/"); break;
	case ID_WWWHUGS: ExecuteFile("http://haskell.org/hugs/"); break;
	case ID_ABOUT: ShowAboutDialog(hWnd); break;
    }
}

int MainNotify(HWND hWnd, LPNMHDR nmhdr)
{
    if (nmhdr->code == TBN_GETINFOTIP && nmhdr->idFrom == ID_TOOLBAR) {
	LPNMTBGETINFOTIP tt = (LPNMTBGETINFOTIP) nmhdr;
	LoadString(hThisInstance, tt->iItem, tt->pszText, tt->cchTextMax);
    }
    else if (nmhdr->idFrom == ID_RTF)
	return RtfNotify(hWnd, nmhdr);

    return FALSE;
}

void SetStatusBar(LPCTSTR Str)
{
    SetDlgItemText(hThisWindow, ID_STATUS, Str);
}

void MainMenuSelect(HWND hWnd, int ID, int Flags)
{
    CHAR Buffer[100];

    if (Flags & MF_POPUP || Flags == 0xFFFF)
	ID = 0;

    if (ID == 0 || !LoadString(hThisInstance, ID, Buffer, sizeof(Buffer)))
	Buffer[0] = 0;

    SetStatusBar(Buffer);
}

void MainDropFiles(HWND hWnd, HDROP hDrop)
{
    char Command[MAX_PATH], File[MAX_PATH];

    DragQueryFile(hDrop, 0, File, MAX_PATH);
    DragFinish(hDrop);

    //Move the current directory
    //Happens automatically if they use the open dialog
    //If they directly invoke :load then not necessary
    SetWorkingDir(File);

    wsprintf(Command, ":load %s", ExpandFileName((String)File));
    FireCommand(Command);
}

void ShowContextMenu(int x, int y)
{
    HMENU hEdit = GetSubMenu(GetMenu(hThisWindow), 1);

    if (x == 0xffff && y == 0xffff)
    {
	RECT rc;
        GetWindowRect(GetDlgItem(hThisWindow, ID_RTF), &rc);
	x = rc.left+2;
	y = rc.top+2;
    }

    TrackPopupMenu(hEdit, 0, x, y, 0, hThisWindow, NULL);
    CreatePopupMenu();
}

INT_PTR CALLBACK MainDlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg) {
	case WM_INITDIALOG:
	    MainInitDialog(hWnd);
	    break;

	case WM_DROPFILES:
	    MainDropFiles(hWnd, (HDROP) wParam);
	    break;

	case WM_APP:
	    FireCommand(CommandDelayBuffer);
	    break;

	case WM_COMMAND:
	    MainCommand(hWnd, LOWORD(wParam));
	    break;

	case WM_NOTIFY:
	    return MainNotify(hWnd, (LPNMHDR) lParam);
	    break;

	case WM_SIZE:
	    MainSize(hWnd, LOWORD(lParam), HIWORD(lParam));
	    break;

	case WM_MENUSELECT:
	    MainMenuSelect(hWnd, LOWORD(wParam), HIWORD(wParam));
	    break;

	case WM_TIMER:
	    RtfWindowTimer();;
	    break;

	case WM_CONTEXTMENU:
	    {
		HWND hParam = (HWND) wParam;
		HWND hRtfChild = GetDlgItem(hWnd, ID_RTF);
		if (hParam == hWnd || hParam == hRtfChild)
		    ShowContextMenu(LOWORD(lParam), HIWORD(lParam));
	    }
	    break;

	case WM_HELP:
	    MainCommand(hWnd, ID_HELPCONTENTS);
	    break;

	case WM_CLOSE:
	    RegistryWriteWindowPos(hWnd);
	    if (Running)
		AbortExecution();
	    PostQuitMessage(0);
	    break;
    }

    return FALSE;
}
