

void ShellInit();
void ShellDest();

bool CreateDesktopShortcut(HWND hDlg, char* Folder);
bool CreateStartMenuShortcut(HWND hDlg, char* Folder);
void Browse(HWND hDlg, HWND hText);
void GetProgramFiles(HWND hDlg, char* Buffer);
bool RegisterFiletypes(HWND hDlg, char* Folder);
void RegisterUninstall(HWND hDlg, char* Folder, DWORD Size);
