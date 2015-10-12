#define WIN32_MEAN_AND_LEAN
#include <windows.h>

const int MyMaxPath = MAX_PATH * 2;

typedef DWORD u32;
typedef BYTE u8;
typedef UINT uint;


void InitCRC();
u32 GetCRC();
void CRC(const u8* buf, uint len);

void DeleteOnReboot(LPCTSTR File);
BOOL RegDelnode (HKEY hKeyRoot, LPTSTR lpSubKey);
