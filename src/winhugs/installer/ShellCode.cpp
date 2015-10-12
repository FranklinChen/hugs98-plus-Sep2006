#include "Header.h"
#include "Parameters.h"
#include <shlobj.h>
#include "FileCode.h"
#include "InstallLog.h"


bool OleReady;

void ShellInit()
{
	HRESULT hres = OleInitialize(NULL);
	OleReady = ((hres == S_FALSE) || (hres == S_OK));
}

void ShellDest()
{
	if (OleReady)
		OleUninitialize();
}


bool CreateShortcut(char* Destination, char* Target, char* StartIn, char* Parameters, char* Desc)
{
	if (!OleReady)
		return false;

    HRESULT hres;
    IShellLink* psl;

    // Get a pointer to the IShellLink interface.
    hres = CoCreateInstance(CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER,
                            IID_IShellLink, (LPVOID*)&psl);
    if (SUCCEEDED(hres))
    {
        IPersistFile* ppf;

        // Set the path to the shortcut target and add the description.
        psl->SetPath(Target);
		if (Parameters != NULL) psl->SetArguments(Parameters);
        if (Desc != NULL) psl->SetDescription(Desc);
		if (StartIn != NULL) psl->SetWorkingDirectory(StartIn);

        // Query IShellLink for the IPersistFile interface for saving the
        // shortcut in persistent storage.
        hres = psl->QueryInterface(IID_IPersistFile, (LPVOID*)&ppf);

        if (SUCCEEDED(hres))
        {
            WCHAR wsz[MAX_PATH];

            // Ensure that the string is Unicode.
            MultiByteToWideChar(CP_ACP, 0, Destination, -1, wsz, MAX_PATH);

            // Save the link by calling IPersistFile::Save.
            hres = ppf->Save(wsz, TRUE);
            ppf->Release();
        }
        psl->Release();
    }
    bool Res = (SUCCEEDED(hres) ? true : false);
	if (Res)
		WriteInstallLog("FILE\t%s", Destination);
	return Res;
}


bool GetFolder(HWND hDlg, int nFolder, char* Buffer)
{
	LPITEMIDLIST idl;
	SHGetSpecialFolderLocation(hDlg, nFolder, &idl);
	if (idl == 0) return false;

	BOOL res = SHGetPathFromIDList(idl, Buffer);
	CoTaskMemFree(idl);
	return (res != FALSE);
}


bool CreateDesktopShortcut(HWND hDlg, char* Folder)
{
	char Destination[MyMaxPath];
	if (!GetFolder(hDlg, CSIDL_DESKTOP, Destination))
		return false;

	int i = strlen(Destination);
	if (Destination[i-1] == '\\')
		Destination[i-1] = 0;
	strcat(Destination, "\\" ProgramName ".lnk");

	char Target[MyMaxPath];
	strcpy(Target, Folder);
	strcat(Target, "\\" PrimaryFile);

	return CreateShortcut(Destination, Target, Folder, NULL, ProgramName " - " Description);
}

bool CreateStartMenuShortcut(HWND hDlg, char* Folder)
{
	char Destination[MyMaxPath];
	if (!GetFolder(hDlg, CSIDL_PROGRAMS, Destination))
		return false;

	strcat(Destination, "\\" ProgramName);
	if (!EnsureFolder(Destination))
		return false;

	strcat(Destination, "\\");
	char* i = &Destination[strlen(Destination)];

	char Target[MyMaxPath];
	strcpy(Target, Folder);
	strcat(Target, "\\" PrimaryFile);

	strcpy(i, ProgramName ".lnk");
	bool res = CreateShortcut(Destination, Target, Folder, NULL, ProgramName " - " Description);

	strcpy(i, "Readme.lnk");
	strcpy(&Target[strlen(Folder)+1], "readme.txt");
	res &= CreateShortcut(Destination, Target, NULL, NULL, ProgramName " - Read Me");

	return res;
}

void WriteRegistryLog(HKEY Root, char* Path)
{
	char* RootName;
	if (Root == HKEY_CLASSES_ROOT)
		RootName = "HKEY_CLASSES_ROOT";
	else if (Root == HKEY_LOCAL_MACHINE)
		RootName = "HKEY_LOCAL_MACHINE";
	else
		RootName = "<UNKNOWN>";

	WriteInstallLog("REG\t%s\t%s", RootName, Path);
}

void WriteRegistryNum(HKEY Root, char* Path, char* Local, DWORD Value)
{
	HKEY hKey;
	RegCreateKey(Root, Path, &hKey);
	if (hKey != NULL)
	{
		RegSetValueEx(hKey, Local, 0, REG_DWORD, (BYTE*) &Value, sizeof(Value));
		RegCloseKey(hKey);
		WriteRegistryLog(Root, Path);
	}
}

void WriteRegistry(HKEY Root, char* Path, char* Local, char* Value)
{
	HKEY hKey;
	RegCreateKey(Root, Path, &hKey);
	if (hKey != NULL)
	{
		RegSetValueEx(hKey, Local, 0, REG_SZ, (BYTE*) Value, strlen(Value)+1);
		RegCloseKey(hKey);
		WriteRegistryLog(Root, Path);
	}
}

bool RegisterFiletypes(HWND hDlg, char* Folder)
{
#define HASKELL_HANDLER "hugs_haskell"

	char Buffer[MyMaxPath];
	Buffer[0] = '\"';
	strcpy(&Buffer[1], Folder);
	char* FileName = &Buffer[strlen(Folder)+1];
	FileName[0] = '\\';
	FileName++;

	//Register the two extensions
	WriteRegistry(HKEY_CLASSES_ROOT, ".hs" , "", HASKELL_HANDLER);
	WriteRegistry(HKEY_CLASSES_ROOT, ".lhs", "", HASKELL_HANDLER);

	//Allow the user to create a template
	WriteRegistry(HKEY_CLASSES_ROOT, ".hs\\ShellNew", "FileName", "");


	WriteRegistry(HKEY_CLASSES_ROOT, HASKELL_HANDLER,                 "", "Haskell Script");
	strcpy(FileName, PrimaryFile "\",1");
	WriteRegistry(HKEY_CLASSES_ROOT, HASKELL_HANDLER "\\DefaultIcon", "", Buffer);
	WriteRegistry(HKEY_CLASSES_ROOT, HASKELL_HANDLER "\\shell",       "", "");

	strcpy(FileName, PrimaryFile "\" \"%1\"");
	WriteRegistry(HKEY_CLASSES_ROOT, HASKELL_HANDLER "\\shell\\Open",          "", "");
	WriteRegistry(HKEY_CLASSES_ROOT, HASKELL_HANDLER "\\shell\\Open\\command", "", Buffer);

	strcpy(FileName, PrimaryFile "\" /edit \"%1\"");
	WriteRegistry(HKEY_CLASSES_ROOT, HASKELL_HANDLER "\\shell\\Edit",          "", "");
	WriteRegistry(HKEY_CLASSES_ROOT, HASKELL_HANDLER "\\shell\\Edit\\command", "", Buffer);

	return true;
}

void RegisterUninstall(HWND hDlg, char* Folder, DWORD Size)
{
#define UNINSTALL_ENTRY "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\" ProgramName

	char Buffer[MyMaxPath];
	Buffer[0] = '\"';
	strcpy(&Buffer[1], Folder);
	char* FileName = &Buffer[strlen(Folder)+1];
	FileName[0] = '\\';
	FileName++;

	WriteRegistry(HKEY_LOCAL_MACHINE, UNINSTALL_ENTRY, "DisplayName", ProgramName);
	strcpy(FileName, PrimaryFile "\",0");
	WriteRegistry(HKEY_LOCAL_MACHINE, UNINSTALL_ENTRY, "DisplayIcon", Buffer);
	strcpy(FileName, "uninstaller.exe\""); 
	WriteRegistry(HKEY_LOCAL_MACHINE, UNINSTALL_ENTRY, "UninstallString", Buffer);
	WriteRegistry(HKEY_LOCAL_MACHINE, UNINSTALL_ENTRY, "Publisher", Publisher);
	WriteRegistry(HKEY_LOCAL_MACHINE, UNINSTALL_ENTRY, "HelpLink", Website);

	WriteRegistryNum(HKEY_LOCAL_MACHINE, UNINSTALL_ENTRY, "EstimatedSize", Size);
}

void GetProgramFiles(HWND hDlg, char* Buffer)
{
	char* s = getenv("PROGRAMFILES");
	strcpy(Buffer, (s != NULL ? s : "C:\\Program Files"));
}



int CALLBACK BrowseCallbackProc(HWND hWnd, UINT uMsg, LPARAM lParam, LPARAM lpData)
{
	switch(uMsg)
	{
	case BFFM_INITIALIZED:
		char Buffer[MyMaxPath];
		GetWindowText((HWND) lpData, Buffer, MyMaxPath);
		SendMessage(hWnd, BFFM_SETSELECTION, TRUE, (LPARAM) Buffer);
		break;
	}

	return 0;
}


void Browse(HWND hDlg, HWND hText)
{
	const int bif_NEWDIALOGSTYLE = 0x40;

	BROWSEINFO bi;
	bi.hwndOwner = hDlg;
	bi.pidlRoot = NULL;
	bi.pszDisplayName = NULL;
	bi.lpszTitle = "Select the installation folder for " ProgramName;
	bi.ulFlags = BIF_RETURNONLYFSDIRS | bif_NEWDIALOGSTYLE;
	bi.lpfn = &BrowseCallbackProc;
	bi.lParam = (LPARAM) hText;
	bi.iImage = 0;

	LPITEMIDLIST idl = SHBrowseForFolder(&bi);

	if (idl != NULL)
	{
		char Buffer[MyMaxPath];
		SHGetPathFromIDList(idl, Buffer);
		SetWindowText(hText, Buffer);
		CoTaskMemFree(idl);
	}
}
