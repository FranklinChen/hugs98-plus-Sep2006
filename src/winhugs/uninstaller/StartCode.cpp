#include "header.h"
#include <commctrl.h>
#include <stdio.h>
#include "LogReader.h"
#include "LinkedList.h"

#include "resource.h"
#include "Parameters.h"



#define ErrBox(Msg)				MessageBox(hDlg, Msg, ProgramName " Installer", MB_ICONERROR)
#define QuestBox(Msg, Flag)		MessageBox(hDlg, Msg, ProgramName " Installer", MB_ICONQUESTION | Flag)
#define InfoBox(Msg)			MessageBox(hDlg, Msg, ProgramName " Installer", MB_ICONINFORMATION)

// GLOBAL STATE
HINSTANCE hInst;
bool InDoEvents = false;
Log* log;
int State;
// END GLOBAL STATE


void CheckDeleteRights(HWND hDlg);
void PerformUninstall(HWND hDlg);

void DoEvents()
{
	InDoEvents = true;
	MSG msg;
	while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
	{
        TranslateMessage(&msg);
        DispatchMessage(&msg);
	}
	InDoEvents = false;
}

bool FileExists(LPCTSTR File)
{
	return (GetFileAttributes(File) == INVALID_FILE_ATTRIBUTES ? false : true);
}

void PaintDialog(HWND hDlg)
{
	const char* Msg = ProgramName " - " Description "\n© " Copyright;
	static int MsgLen = strlen(Msg);

	PAINTSTRUCT ps;
	HDC hDC = BeginPaint(hDlg, &ps);

	SelectObject(hDC, GetStockObject(DEFAULT_GUI_FONT));

	RECT rc = {0, 0, 463, 54};

	Rectangle(hDC, rc.left, rc.top, rc.right, rc.bottom);
	FillRect(hDC, &rc, (HBRUSH) GetStockObject(WHITE_BRUSH));
	rc.top = 13;
	DrawText(hDC, Msg, MsgLen, &rc, DT_WORDBREAK | DT_CENTER);

	EndPaint(hDlg, &ps);
}

void InitDialog(HWND hDlg)
{
	InitCommonControls();
}

u32 CalcCRC(char* FileName)
{
	HANDLE hFile = CreateFile(FileName, GENERIC_READ, FILE_SHARE_READ,
		NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (hFile == INVALID_HANDLE_VALUE)
		return 0;

	DWORD Size = GetFileSize(hFile, NULL);
	InitCRC();
	while (Size != 0)
	{
		DWORD dw;
		u8 Buffer[10000];
		DWORD toRead = min(10000, Size);
		ReadFile(hFile, Buffer, toRead, &dw, NULL);
		Size -= dw;
		if (dw == 0)
		{
			CloseHandle(hFile);
			return 0;
		}
		CRC(Buffer, dw);
	}
	CloseHandle(hFile);
	return GetCRC();
}

void EnableDlgItem(HWND hDlg, int ID, bool Enable)
{
	EnableWindow(GetDlgItem(hDlg, ID), (Enable ? TRUE : FALSE));
}

void ShowDlgItem(HWND hDlg, int ID, bool Show)
{
	ShowWindow(GetDlgItem(hDlg, ID), (Show ? SW_SHOW : SW_HIDE));
}


void CheckModified(HWND hDlg)
{
	ShowDlgItem(hDlg, lblWelcome, false);
	EnableDlgItem(hDlg, IDOK, false);
	EnableDlgItem(hDlg, IDCANCEL, false);

	SetDlgItemText(hDlg, lblMessage, "Checking for modified files");
	SendDlgItemMessage(hDlg, prgBar, PBM_SETRANGE, 0, MAKELPARAM(0, log->nFile));
	ShowDlgItem(hDlg, lblMessage, true);
	ShowDlgItem(hDlg, prgBar, true);

	int Modified = 0;

	for (int i = 0; i < log->nFile; i++)
	{
		DoEvents();
		File* f = log->Files[i];
		f->Modified = false;
		if (f->CRC != 0)
		{
			u32 CRC = CalcCRC(f->FileName);
			if (CRC != 0 && f->CRC != CRC)
			{
				f->Modified = true;
				Modified++;
				SendDlgItemMessage(hDlg, lstItems, LB_ADDSTRING, 0, (LPARAM) f->FileName);
			}
		}
		SendDlgItemMessage(hDlg, prgBar, PBM_SETPOS, i, 0);
	}
	if (Modified != 0)
	{
		SetDlgItemText(hDlg, lblMessage, "Modified files found");
		ShowDlgItem(hDlg, prgBar, false);
		EnableDlgItem(hDlg, IDOK, true);
		EnableDlgItem(hDlg, IDCANCEL, true);
		ShowDlgItem(hDlg, lstItems, true);
		ShowDlgItem(hDlg, chkDeleteModified, true);
	}
	else
	{
		State++;
		CheckDeleteRights(hDlg);
	}
}

void CheckDeleteRights(HWND hDlg)
{
	EnableDlgItem(hDlg, IDOK, false);
	EnableDlgItem(hDlg, IDCANCEL, false);

	SetDlgItemText(hDlg, lblMessage, "Checking for locked files");
	SendDlgItemMessage(hDlg, prgBar, PBM_SETRANGE, 0, MAKELPARAM(0, log->nFile));
	ShowDlgItem(hDlg, lblMessage, true);
	ShowDlgItem(hDlg, prgBar, true);
	ShowDlgItem(hDlg, lstItems, false);
	SendDlgItemMessage(hDlg, lstItems, LB_RESETCONTENT, 0, 0);
	ShowDlgItem(hDlg, chkDeleteModified, false);

	int Locked = 0;

	for (int i = 0; i < log->nFile; i++)
	{
		DoEvents();
		File* f = log->Files[i];

		HANDLE hFile = CreateFile(f->FileName, GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
		if (hFile != INVALID_HANDLE_VALUE)
			CloseHandle(hFile);
		else if (FileExists(f->FileName))
		{
			Locked++;
			SendDlgItemMessage(hDlg, lstItems, LB_ADDSTRING, 0, (LPARAM) f->FileName);
		}
		SendDlgItemMessage(hDlg, prgBar, PBM_SETPOS, i, 0);
	}
	if (Locked != 0)
	{
		SetDlgItemText(hDlg, lblMessage, "The following files appear to be in use, continue?");
		ShowDlgItem(hDlg, prgBar, false);
		EnableDlgItem(hDlg, IDOK, true);
		EnableDlgItem(hDlg, IDCANCEL, true);
		ShowDlgItem(hDlg, lstItems, true);
	}
	else
	{
		State++;
		PerformUninstall(hDlg);
	}
}

int compareStr(const void* a, const void* b)
{
	return -strcmp(*((const char**) a), *((const char**) b));
}

void PerformUninstall(HWND hDlg)
{
	EnableDlgItem(hDlg, IDOK, false);
	EnableDlgItem(hDlg, IDCANCEL, false);

	SetDlgItemText(hDlg, lblMessage, "Deleting files");
	SendDlgItemMessage(hDlg, prgBar, PBM_SETRANGE, 0, MAKELPARAM(0, log->nFile));
	ShowDlgItem(hDlg, lblMessage, true);
	ShowDlgItem(hDlg, prgBar, true);
	ShowDlgItem(hDlg, lstItems, false);
	SendDlgItemMessage(hDlg, lstItems, LB_RESETCONTENT, 0, 0);

	bool DeleteModified = (IsDlgButtonChecked(hDlg, chkDeleteModified) == BST_CHECKED);

	LinkedList* Dirs = NULL;

	int Alive = 0;

	for (int i = 0; i < log->nFile; i++)
	{
		DoEvents();
		File* f = log->Files[i];

		if (!f->Modified || DeleteModified)
			f->Modified = (DeleteFile(f->FileName) ? false : true);
		if (f->Modified)
		{
			if (FileExists(f->FileName))
			{
				Alive++;
				SendDlgItemMessage(hDlg, lstItems, LB_ADDSTRING, 0, (LPARAM) f->FileName);
			}
			else
				f->Modified = false;
		}
		if (!f->Modified)
		{
			// calculate all parent directories
			char Buffer[MAX_PATH], Buffer2[MAX_PATH];
			char* in = Buffer;
			char* out = Buffer2;
			strcpy(in, f->FileName);
			char* c;

			while(strlen(in) > 2)
			{
				int res = GetFullPathName(in, MAX_PATH, out, &c);
				if (res == 0 || c == NULL) break;
				c[-1] = 0;
				Dirs = NewLinkedList(strdup(out), Dirs);

				char* t = in;
				in = out;
				out = t;
			}
		}

		SendDlgItemMessage(hDlg, prgBar, PBM_SETPOS, i, 0);
	}

	//now go on a directory hunt :)
	int n;
	char** DirList = (char**) LinkedListToArray(Dirs, &n);
	qsort(DirList, n, sizeof(char*), compareStr);

	//now delete all dupes
	int j = 0;
	for (int i = 1; i < n; i++)
	{
		if (strcmp(DirList[j], DirList[i]) != 0)
			DirList[++j] = DirList[i];
	}
	n = j + 1;

	SetDlgItemText(hDlg, lblMessage, "Deleting directories");
	for (int i = 0; i < n; i++)
	{
		DoEvents();
		RemoveDirectory(DirList[i]);
	}

	SetDlgItemText(hDlg, lblMessage, "Deleting registry keys");
	for (int i = 0; i < log->nRegistry; i++)
	{
		DoEvents();
		RegDelnode(log->Registrys[i]->Root, log->Registrys[i]->Path);
	}

	if (Alive != 0)
	{
		SetDlgItemText(hDlg, lblMessage, "The following files were not deleted");
		ShowDlgItem(hDlg, lstItems, true);
	}
	else
	{
		SetDlgItemText(hDlg, lblMessage, "WinHugs successfully uninstalled");
	}

	ShowDlgItem(hDlg, prgBar, false);
	EnableDlgItem(hDlg, IDOK, true);
	SetDlgItemText(hDlg, IDOK, "Finish");
}

int CALLBACK DlgFunc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch(uMsg)
	{
	case WM_PAINT:
		PaintDialog(hDlg);
		break;

	case WM_COMMAND:
		switch (LOWORD(wParam))
		{
		case IDCANCEL:
			EndDialog(hDlg, 0);
			break;

		case IDOK:
			State++;
			switch (State)
			{
			case 1:
				CheckModified(hDlg);
				break;

			case 2:
				CheckDeleteRights(hDlg);
				break;

			case 3:
				PerformUninstall(hDlg);
				break;

			case 4:
				EndDialog(hDlg, 0);
				break;
			}
			break;
		}
	}

	return FALSE;
}

// Supported command lines
// "" - the user ran the instance next to install.og
// /del file - delete the file and exit
// /run file - run with file as the uninstaller
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	char Self[MyMaxPath];
	GetModuleFileName(hInstance, Self, MyMaxPath);

	if (strncmp(lpCmdLine, "/run ", 5) == 0)
	{
		char* OrigExe = &lpCmdLine[5];
		char Buffer[MyMaxPath];
		char* s;
		GetFullPathName(OrigExe, MyMaxPath, Buffer, &s);

		if (s == NULL)
			log = NULL;
		else
		{
			strcpy(s, "install.log");
			log = ReadLog(Buffer, s);
		}

		if (log == NULL)
			MessageBox(NULL, "Failed to load uninstall file \"install.log\"", "WinHugs Uninstaller", MB_ICONERROR);	
		else
		{
			InitCommonControls();
			DialogBox(hInstance, MAKEINTRESOURCE(dlgInstall), NULL, DlgFunc);
		}

		bool Success = false;
		if (FileExists(OrigExe))
		{
			strcpy(Buffer, "/del ");
			strcat(Buffer, Self);
			if ((int) ShellExecute(NULL, NULL, OrigExe, Buffer, NULL, SW_NORMAL) > 32)
				Success = true;
		}
		if (!Success)
			DeleteOnReboot(Self);
	}
	else if (strncmp(lpCmdLine, "/del ", 5) == 0)
	{
		//try and delete the file
		char* File = &lpCmdLine[5];
		for (int i = 0; i < 30; i++)
		{
			DeleteFile(File);
			if (!FileExists(File))
				break;
		}
	}
	else
	{
		char TempPath[MyMaxPath], TempFile[MyMaxPath];
		GetTempPath(MyMaxPath, TempPath);
		GetTempFileName(TempPath, "HUG", 0, TempFile);
		strcat(TempFile, ".exe");

		bool Success = false;
		if (CopyFile(Self, TempFile, FALSE))
		{
			char CmdLine[MyMaxPath];
			strcpy(CmdLine, "/run ");
			strcat(CmdLine, Self);
			if ((int) ShellExecute(NULL, NULL, TempFile, CmdLine, TempPath, nCmdShow) > 32)
				Success = true;
		}

		if (!Success)
		{
			MessageBox(NULL, "Failed to launch the uninstaller", "WinHugs Uninstaller", MB_ICONERROR);	
		}
	}
	return 0;
}
