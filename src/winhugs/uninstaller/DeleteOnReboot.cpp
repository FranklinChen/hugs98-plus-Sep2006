
#include "Header.h"

// Code take from NSIS, which has a compatible license
// http://nsis.sourceforge.net/

void MoveFileOnReboot(LPCTSTR pszExisting, LPCTSTR pszNew);

void DeleteOnReboot(LPCTSTR File)
{
	MoveFileOnReboot(File, NULL);
}

#define NSISCALL 
#define CHAR2_TO_WORD(a,b) (((WORD)(a))|((b)<<8))
#define CHAR4_TO_DWORD(a,b,c,d)	(((DWORD)CHAR2_TO_WORD(a,b))|(CHAR2_TO_WORD(c,d)<<16))


void * NSISCALL myGetProcAddress(char *dll, char *func)
{
  HMODULE hModule = GetModuleHandle(dll);
  if (!hModule)
    hModule = LoadLibrary(dll);
  if (!hModule)
    return NULL;

  return GetProcAddress(hModule, func);
}

HANDLE NSISCALL myOpenFile(const char *fn, DWORD da, DWORD cd)
{
  int attr = GetFileAttributes(fn);
  return CreateFile(
    fn,
    da,
    FILE_SHARE_READ,
    NULL,
    cd,
    attr == INVALID_FILE_ATTRIBUTES ? 0 : attr,
    NULL
  );
}

char * NSISCALL mystrcat(char *out, const char *concat)
{
  return lstrcat(out, concat);
}

int NSISCALL mystrlen(const char *in)
{
  return lstrlen(in);
}

char * NSISCALL mystrstri(char *a, char *b)
{
  int l = mystrlen(b);
  while (mystrlen(a) >= l)
  {
    char c = a[l];
    a[l] = 0;
    if (!lstrcmpi(a, b))
    {
      a[l] = c;
      return a;
    }
    a[l] = c;
    a = CharNext(a);
  }
  return NULL;
}

char * NSISCALL mystrcpy(char *out, const char *in)
{
  return lstrcpy(out, in);
}

void mini_memcpy(void *o,void*i,int l)
{
  char *oo=(char*)o;
  char *ii=(char*)i;
  while (l-- > 0) *oo++=*ii++;
}

void NSISCALL MoveFileOnReboot(LPCTSTR pszExisting, LPCTSTR pszNew)
{
  BOOL fOk = 0;
  typedef BOOL (WINAPI *mfea_t)(LPCSTR lpExistingFileName,LPCSTR lpNewFileName,DWORD dwFlags);
  mfea_t mfea;
  mfea=(mfea_t) myGetProcAddress("KERNEL32.dll","MoveFileExA");
  if (mfea)
  {
    fOk=mfea(pszExisting, pszNew, MOVEFILE_DELAY_UNTIL_REBOOT|MOVEFILE_REPLACE_EXISTING);
  }

  if (!fOk)
  {
    static char szRenameLine[1024];
    static char wininit[1024];
    static char tmpbuf[1024];
    int cchRenameLine;
    char *szRenameSec = "[Rename]\r\n";
    HANDLE hfile, hfilemap;
    DWORD dwFileSize, dwRenameLinePos;

    int spn;

    *(DWORD*)tmpbuf = CHAR4_TO_DWORD('N', 'U', 'L', 0);

    if (pszNew) {
      // create the file if it's not already there to prevent GetShortPathName from failing
      CloseHandle(myOpenFile(pszNew,0,CREATE_NEW));
      spn = GetShortPathName(pszNew,tmpbuf,1024);
      if (!spn || spn > 1024)
        return;
    }
    // wininit is used as a temporary here
    spn = GetShortPathName(pszExisting,wininit,1024);
    if (!spn || spn > 1024)
      return;
    cchRenameLine = wsprintf(szRenameLine,"%s=%s\r\n",tmpbuf,wininit);

    GetWindowsDirectory(wininit, 1024-16);
    mystrcat(wininit, "\\wininit.ini");
    hfile = CreateFile(wininit,
        GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_ALWAYS,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, NULL);

    if (hfile != INVALID_HANDLE_VALUE)
    {
      dwFileSize = GetFileSize(hfile, NULL);
      hfilemap = CreateFileMapping(hfile, NULL, PAGE_READWRITE, 0, dwFileSize + cchRenameLine + 10, NULL);

      if (hfilemap != NULL)
      {
        LPSTR pszWinInit = (LPSTR) MapViewOfFile(hfilemap, FILE_MAP_WRITE, 0, 0, 0);

        if (pszWinInit != NULL)
        {
          LPSTR pszRenameSecInFile = mystrstri(pszWinInit, szRenameSec);
          if (pszRenameSecInFile == NULL)
          {
            mystrcpy(pszWinInit+dwFileSize, szRenameSec);
            dwFileSize += 10;
            dwRenameLinePos = dwFileSize;
          }
          else
          {
            char *pszFirstRenameLine = pszRenameSecInFile+10;
            char *pszNextSec = mystrstri(pszFirstRenameLine,"\n[");
            if (pszNextSec)
            {
              char *p = ++pszNextSec;
              while (p < pszWinInit + dwFileSize) {
                p[cchRenameLine] = *p;
                p++;
              }

              dwRenameLinePos = pszNextSec - pszWinInit;
            }
            // rename section is last, stick item at end of file
            else dwRenameLinePos = dwFileSize;
          }

          mini_memcpy(&pszWinInit[dwRenameLinePos], szRenameLine, cchRenameLine);
          dwFileSize += cchRenameLine;

          UnmapViewOfFile(pszWinInit);

          //fOk++;
        }
        CloseHandle(hfilemap);
      }
      SetFilePointer(hfile, dwFileSize, NULL, FILE_BEGIN);
      SetEndOfFile(hfile);
      CloseHandle(hfile);
    }
  }
  //return fOk;
}
