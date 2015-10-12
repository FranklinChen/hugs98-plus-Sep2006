#define WIN32_MEAN_AND_LEAN
//#define UNICODE
//#define _UNICODE
#include <windows.h>
#include <richedit.h>
#include <commctrl.h>
#include <tchar.h>

// Globally shared variables
extern HINSTANCE hThisInstance;
extern HWND hThisWindow;
extern HACCEL hAccelTable;

// Exported from General.c
void ExecuteFile(LPSTR FileName);
void CenterDialogInParent(HWND hDlg);
void ExecuteFileDocs(LPSTR FileName);
BOOL ShowOpenFileDialog(HWND hParent, LPSTR FileName);

// Exported from Registry.c
void RegistryReadFont(CHARFORMAT* cf);
void RegistryWriteFont(CHARFORMAT* cf);
void RegistryReadWindowPos(HWND hWnd);
void RegistryWriteWindowPos(HWND hWnd);
void RegistryReadMru(LPSTR* Buffer);
void RegistryWriteMru(LPSTR* Buffer);

// Exported from RtfWindow.c
void RtfWindowPutS(LPCTSTR s);
void RtfWindowSetCommand(LPCTSTR s);
void RtfWindowGetCommand(LPTSTR s);
void RtfWindowUpdateFont();
void RtfWindowInit(HWND hNewRTF);
void RtfWindowTimer(); //kind of internal
void RtfWindowFlushBuffer();

// Exported from DlgMain.c
void FireCommandDelay(LPCTSTR Command);
void FireCommand(LPCTSTR Command);
void ExecutionFinished();
BOOL ShowMainDialog();
void EnableButtons();
void SetStatusBar(LPCTSTR Str);
extern BOOL Running;

// From MruFiles.c
LPSTR MruGetItem(int i);
void MruInit();

// Generally around the place
void ShowAboutDialog(HWND hParent);
BOOL ShowOptionsDialog(HWND hParent);
LPSTR ExpandFileName(LPSTR what);
LPCTSTR GetEditor(int Index, LPTSTR Buffer);

// IORemap, for getChar support
void WinHugsReceiveC(int c);
void IORemapBegin();
void IORemapEnd();

// From WinBrowse2.c
void DrawClassesHierarchy();
void DoBrowseClasses();
void DoBrowseNames();
void DoBrowseTycons();
void ShowScriptMan();

// From History.c
void AddHistory(LPCSTR Item);
LPCSTR GetHistory(int delta);
