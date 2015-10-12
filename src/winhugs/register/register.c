#include <stdio.h>

#include <windows.h>
#include <shlobj.h>

#include "..\..\version.h"

typedef CHAR    *String;

#define SLASH    	'\\'

#define HugsRoot                ("SOFTWARE\\Haskell\\Hugs\\" HUGS_VERSION "\\")
#define WinhugsRoot  	        ("SOFTWARE\\Haskell\\Hugs\\Winhugs" HUGS_VERSION "\\")


#define HASKELL_SCRIPT          "haskellProgram"
#define TEMPLATE                expandHugsPath("{Hugs}\\template.hs")

#define HASKELL_SCRIPT_ICON     expandHugsPath("{Hugs}\\icons\\hsicon.ico")
#define HUGS_EXE_ICON           expandHugsPath("{Hugs}\\icons\\hugsicon.ico")  

#define WINHUGS_EXE             expandHugsPath("{Hugs}\\winhugs.exe")
#define HUGS_EXE                expandHugsPath("{Hugs}\\hugs.exe")


#define PROGRAM_GROUP           "Hugs 98"


#define HUGSPATH 		"{Hugs}\\libraries;{Hugs}\\packages\\*"


static char hugsHome[MAX_PATH]       = "";  /* hugs folder              */
static char programsFolder[MAX_PATH] = "";  /* Start -> Programs folder */


/* get path to Start -> Programs folder */
void getProgramsFolder(String str)
{
  LPITEMIDLIST   pidlStartMenu;

  //get the pidl for the start menu - thgis will be used to intialize the folder browser
  SHGetSpecialFolderLocation(NULL, CSIDL_PROGRAMS, &pidlStartMenu);

  //get the path for the folder
  SHGetPathFromIDList(pidlStartMenu, str);

}

/* Creates a new folder */
void CreateFolder(String folder) 
{ 
  //create the folder
  CreateDirectory(folder, NULL);

  //notify the shell that you made a change
  SHChangeNotify(SHCNE_MKDIR, SHCNF_PATH, folder, 0);
}


// CreateLink - uses the shell's IShellLink and IPersistFile interfaces 
//   to create and store a shortcut to the specified object. 
// Returns the result of calling the member functions of the interfaces. 
HRESULT CreateLink(String destPath, String groupPath, String newLinkPath, String args, String iconPath) 
{ 
    HRESULT hres; 
    IShellLink* psl; 
    char groupAndLinkPath[MAX_PATH];

    sprintf(groupAndLinkPath, "%s\\%s", groupPath, newLinkPath);

    // Get a pointer to the IShellLink interface. 
    hres = CoCreateInstance(&CLSID_ShellLink, NULL, 
        CLSCTX_INPROC_SERVER, &IID_IShellLink, &psl); 

    if (SUCCEEDED(hres)) { 
        IPersistFile* ppf; 
        // Set the path to the shortcut target, and add the 
        // description. 
        psl->lpVtbl->SetPath(psl, destPath); 
        psl->lpVtbl->SetArguments(psl, args); 
        if (iconPath != NULL) {
          psl->lpVtbl->SetIconLocation(psl, iconPath, 0); 
        }
 
       // Query IShellLink for the IPersistFile interface for saving the 
       // shortcut in persistent storage. 
        hres = psl->lpVtbl->QueryInterface(psl, &IID_IPersistFile, 
            &ppf); 
  
        if (SUCCEEDED(hres)) { 
            WORD wsz[MAX_PATH]; 
 
            // Ensure that the string is ANSI. 
            MultiByteToWideChar(CP_ACP, 0, groupAndLinkPath, -1, 
                wsz, MAX_PATH); 
 
            // Save the link by calling IPersistFile::Save. 
            hres = ppf->lpVtbl->Save(ppf, wsz, TRUE); 
            ppf->lpVtbl->Release(ppf); 

            
        } 
        psl->lpVtbl->Release(psl); 
    } 

    return hres; 
} 



static BOOL createKey(HKEY hKey, String regPath, PHKEY phRootKey, REGSAM samDesired) {
    DWORD  dwDisp;
    return RegCreateKeyEx(hKey, regPath,
			  0, "", REG_OPTION_NON_VOLATILE,
			  samDesired, NULL, phRootKey, &dwDisp) 
	   == ERROR_SUCCESS;
}


static BOOL setValue(HKEY hKey, String regPath, String var, DWORD type, LPBYTE buf, DWORD bufSize) {
    HKEY hRootKey;

    if (!createKey(hKey, regPath, &hRootKey, KEY_WRITE)) {
	return FALSE;
    } else {
	LONG res = RegSetValueEx(hRootKey, var, 0, type, buf, bufSize);
	RegCloseKey(hRootKey);
	return (res == ERROR_SUCCESS);
    }
}

/* write String to winhugs registry */
static BOOL writeWinhugsRegString(String var, String val) {

    if (NULL == val) {
	val = "";
    }
    return setValue(HKEY_CURRENT_USER, WinhugsRoot, var, 
		    REG_SZ, (LPBYTE)val, lstrlen(val)+1);
}

/* write String to hugs registry */
static BOOL writeHugsRegString(String var, String val) {

    if (NULL == val) {
	val = "";
    }
    return setValue(HKEY_CURRENT_USER, HugsRoot, var, 
		    REG_SZ, (LPBYTE)val, lstrlen(val)+1);
}

/* write String to HKEY_CLASSES_ROOT */
static BOOL writeHKCRString(String where, String var, String val) {

    if (NULL == val) {
	val = "";
    }
    return setValue(HKEY_CLASSES_ROOT, where, var, 
		    REG_SZ, (LPBYTE)val, lstrlen(val)+1);
}



/* get folder where program is executing */
void getExeHome(String str) {
  String slash;

  /* get .exe full path */
  GetModuleFileName((HMODULE)0,str,FILENAME_MAX+1);
  /* truncate after directory name */
  if (slash = strrchr(str,SLASH))
    *slash = '\0';
  strlwr(str);
}



/* Replaces all occurrences of str "what" by "by" in "in" */
static VOID StrReplace(CHAR *what, CHAR *by, CHAR *in, CHAR *result)
{

  CHAR *ptrIn, *ptrResult;
  UINT  byLength   = strlen(by);
  UINT  whatLength = strlen(what);

  for(ptrIn=in, ptrResult=result; *ptrIn;){
    if(strncmp(ptrIn,what,whatLength)==0) {
    strcpy(ptrResult, by);
      ptrResult += byLength;
      ptrIn     += whatLength;
    }
    else{
      *ptrResult = *ptrIn;
      ptrResult++; 
      ptrIn++;
    }
  } 
  *ptrResult = (CHAR)0;
  

}


/* Expand "{Hugs}" to real path */
static String expandHugsPath(String toExpand)
{
  #define MAX_RESULTS 5

  static CHAR expanded[MAX_RESULTS][2*_MAX_PATH]; 
  static i = 0;

  /* get next slot */
  i++; if(i>=MAX_RESULTS) i = 0;

  StrReplace("{Hugs}", hugsHome, toExpand, expanded[i]);
  return expanded[i];
}


static String expandSlash(String toExpand)
{
  static CHAR expanded[2*_MAX_PATH]; 
  CHAR        *ptr, *ptrDest;

  for(ptr=toExpand, ptrDest=expanded; *ptr; ) {
    if(*ptr == SLASH) {
      ptrDest[0] = SLASH;
      ptrDest[1] = SLASH;
      ptr++;
      ptrDest++;
      ptrDest++;
    }
    else {
      *ptrDest = *ptr;
      ptr++;
      ptrDest++;
    } 
  }

  *ptrDest = '\0';

  return expanded;
}


/* Set an editor and path for hugs and winhugs */
static void setOptions (String editorCmd) 
{
  static CHAR options[4*_MAX_PATH]; 
  
  sprintf(options, "-E%s -P%s", editorCmd, HUGSPATH);	
  writeHugsRegString("Options",options);
  writeWinhugsRegString("Options",options);

}

/* configuration for winvi32 editor */
static String WinVi32(void)
{
  static CHAR editorCmd[2*_MAX_PATH]; 

  sprintf(editorCmd, "\"\\\"%s\\\\editor\\\\winvi\\\\winvi32.exe\\\" +%cd \\\"%cs\\\"\"", expandSlash(hugsHome), '%', '%');

  return editorCmd;
}

/* configuration for pfe32 editor */
static String Pfe32(void)
{
  static CHAR editorCmd[2*_MAX_PATH]; 

  sprintf(editorCmd, "\"\\\"%s\\\\editor\\\\pfe\\\\pfe32.exe\\\" /g %cd \\\"%cs\\\"\"", expandSlash(hugsHome), '%', '%');

  return editorCmd;
}

/* configuration for ultraedit32 editor */
static String Ultraedit32(void)
{
  static CHAR editorCmd[2*_MAX_PATH]; 

  sprintf(editorCmd, "\"\\\"c:\\\\archivos de programa\\\\ultraedt\\\\uedit32.exe\\\" %cs/%cd/1\"", '%', '%');

  return editorCmd;
}

/* configuration for notepad editor */
static String notepad(void)
{
  static CHAR editorCmd[2*_MAX_PATH]; 

  sprintf(editorCmd, "\"notepad.exe %cs\"", '%');

  return editorCmd;
}


/* configuration for yikes editor */
static String Yikes(void)
{
  static CHAR editorCmd[2*_MAX_PATH]; 

  sprintf(editorCmd, "\"\\\"%s\\\\editor\\\\yikes\\\\yikes.exe\\\" /%cd \\\"%cs\\\"\"", expandSlash(hugsHome), '%', '%');

  return editorCmd;
}


int main(int argc,char *argv[]) {

  BOOL usePfe32       = FALSE;
  BOOL useWinVi32     = FALSE;
  BOOL useUltraedit32 = FALSE;
  BOOL useYikes       = FALSE;

  UINT i;
  char groupPath[MAX_PATH];

  CoInitialize(NULL);


  getExeHome(hugsHome);
  getProgramsFolder(programsFolder);

  /* process command line */
  
  for(i=1; i<argc; ++i) {
    if(strcmp(argv[i],"pfe") == 0)
      usePfe32 = TRUE;
    else if (strcmp(argv[i],"vi") == 0)
      useWinVi32 = TRUE;
    else if (strcmp(argv[i],"uedit") == 0)
      useUltraedit32 = TRUE;
    else if (strcmp(argv[i],"yikes") == 0)
      useYikes = TRUE;
  }

  /* Set editor */
  if (usePfe32)
    setOptions(Pfe32());
  else if (useWinVi32)
    setOptions(WinVi32());
  else if (useUltraedit32)
    setOptions(Ultraedit32());
  else if (useYikes)
    setOptions(Yikes());
  else /* default */
    setOptions(notepad());


  /* set path for documentation */
  writeWinhugsRegString("Doc Path","{Hugs}\\docs");

  /* Falta borrar antiguas entradas en registro */


  /* .hs and .lhs file types */
  writeHKCRString(".hs",           "",         HASKELL_SCRIPT);
  writeHKCRString(".lhs",          "",         HASKELL_SCRIPT);
  writeHKCRString(".hs\\ShellNew", "FileName", TEMPLATE);

  writeHKCRString( HASKELL_SCRIPT,                                    "",          "Haskell Script"                          );
  writeHKCRString( HASKELL_SCRIPT "\\DefaultIcon",                    "",          HASKELL_SCRIPT_ICON                       );
  writeHKCRString( HASKELL_SCRIPT "\\shell",                          "",          ""                                        );

  writeHKCRString( HASKELL_SCRIPT "\\shell\\Open",                    "",          ""                                        );
  writeHKCRString( HASKELL_SCRIPT "\\shell\\Open\\command",           "",          expandHugsPath("\"{Hugs}\\winhugs.exe\" \"%1\" ") );

  writeHKCRString( HASKELL_SCRIPT "\\shell\\Open with hugs",          "",          ""                                        );
  writeHKCRString( HASKELL_SCRIPT "\\shell\\Open with hugs\\command", "",          expandHugsPath("\"{Hugs}\\hugs.exe\" \"%1\" ")    );

  writeHKCRString( HASKELL_SCRIPT "\\shell\\Run",                     "",          ""                                        );
  writeHKCRString( HASKELL_SCRIPT "\\shell\\Run\\command",            "",          expandHugsPath("\"{Hugs}\\runhugs.exe\" \"%1\" ")    );


  /* Create program group */
  sprintf(groupPath, "%s\\" PROGRAM_GROUP, programsFolder);
  CreateFolder (groupPath);


  CreateLink(WINHUGS_EXE, groupPath, "winhugs (haskell 98 mode).lnk", "\"+98\"", NULL);
  CreateLink(WINHUGS_EXE, groupPath, "winhugs (hugs mode).lnk",       "\"-98\"", NULL);
  CreateLink(HUGS_EXE,    groupPath, "hugs (haskell 98 mode).lnk",    "+98",     HUGS_EXE_ICON);
  CreateLink(HUGS_EXE,    groupPath, "hugs (hugs mode).lnk",          "-98",     HUGS_EXE_ICON);


  CoUninitialize();
}

