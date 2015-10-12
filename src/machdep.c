/* --------------------------------------------------------------------------
 * Machine dependent code
 * RISCOS specific code provided by Bryan Scatergood, JBS
 * Macintosh specific code provided by Hans Aberg (haberg@matematik.su.se)
 * HaskellScript code and recursive directory search (now just one level)
 *  provided by Daan Leijen (leijen@fwi.uva.nl)
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: machdep.c,v $
 * $Revision: 1.140 $
 * $Date: 2006/05/11 15:14:11 $
 * ------------------------------------------------------------------------*/
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "opts.h"
#include "strutil.h"
#include "machdep.h"
#include "char.h"
#include "evaluator.h" /* everybody() proto only */

/*#define DEBUG_SEARCH*/

#if HAVE_SIGNAL_H
# include <signal.h>
#endif
#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#else
# if HAVE_TYPES_H
#  include <types.h>
# endif
#endif
#if HAVE_SYS_PARAM_H
# include <sys/param.h>
#endif
#if HAVE_LIMITS_H
# include <limits.h>
#endif
#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#else
# if HAVE_STAT_H
#  include <stat.h>
# endif
#endif
#if HAVE_DIRENT_H
#  include <dirent.h>
#endif
#if HAVE_LOCALE_H
#  include <locale.h>
#endif

/* Hack for systems with unlimited path length (e.g. the Hurd), which
 * will not define PATH_MAX or MAXPATHLEN.  The Right Thing would be
 * to dynamically allocate these buffers, and use functions like
 * get_current_dir_name() and canonicalize_file_name().
 */
#if HAVE_REALPATH && !defined(MAXPATHLEN)
#ifdef PATH_MAX
#define MAXPATHLEN PATH_MAX
#else
#define MAXPATHLEN 4096
#endif
#endif

/* Windows/DOS include files */
#if HAVE_DOS_H
# include <dos.h>
#endif
#if HAVE_CONIO_H && ! HUGS_FOR_WINDOWS
# include <conio.h>
#endif
#if HAVE_IO_H
# include <io.h>
#endif
#if HAVE_STD_H
# include <std.h>
#endif
#if HAVE_WINDOWS_H
# include <windows.h>
#endif

#if DOS
#include <mem.h>
extern unsigned _stklen = 8000;         /* Allocate an 8k stack segment    */
#endif

#if RISCOS
#include "swis.h"
#include "os.h"
#endif

/* Macintosh include files */
#if HAVE_CONSOLE_H
# include <console.h>
#endif
#if HAVE_FILES_H
# include <Files.h>
#endif
#if HAVE_FCNTL_H
# include <fcntl.h>
#endif
#if HAVE_ERRNO_H
# include <errno.h>
#endif
#if HAVE_STDLIB_H
# include <stdlib.h>
#endif
#if HAVE_UNIX_H
#include <unix.h>
#endif

#if __MWERKS__ && macintosh
#include <SIOUX.h>

/*	The variable time_release should be set to a value which gives
	good cooperative multitasking.
*/
int time_release = 20000;
int allow_break_count = 0;
#endif

/* --------------------------------------------------------------------------
 * Prototypes for registry reading
 * ------------------------------------------------------------------------*/
#if USE_REGISTRY

/* where have we hidden things in the registry? */
#if HSCRIPT
#define HScriptRoot ("SOFTWARE\\Haskell\\HaskellScript\\")
#endif

extern String readRegString        Args((HKEY, String, String, String));

static Bool   local createKey      Args((HKEY, String, PHKEY, REGSAM));
static Bool   local queryValue     Args((HKEY, String, String, LPDWORD, LPBYTE, DWORD));
static Bool   local queryString    Args((HKEY,String,String,String*));
static Bool   local setValue       Args((HKEY, String, String, DWORD, LPBYTE, DWORD));
#endif

/* --------------------------------------------------------------------------
 * Find information about a file:
 * ------------------------------------------------------------------------*/

Void getFileInfo(f,tm,sz)  /* find time stamp and size of file*/
String f;
Time   *tm;
Long   *sz; {
#if HAVE_SYS_STAT_H || HAVE_STAT_H || HAVE_UNIX_H
    struct stat scbuf;
    if (!stat(f,&scbuf)) {
	*tm = scbuf.st_mtime;
	*sz = (Long)(scbuf.st_size);
    } else {
	*tm = 0;
	*sz = 0;
    }
#else                                   /* normally just use stat()        */
    os_regset r;                        /* RISCOS PRM p.850 and p.837      */
    r.r[0] = 17;                        /* Read catalogue, no path         */
    r.r[1] = (int)s;
    os_swi(OS_File, &r);
    if(r.r[0] == 1 && (r.r[2] & 0xFFF00000) == 0xFFF00000) {
	tm->hi = r.r[2] & 0xFF;         /* Load address (high byte)        */
	tm->lo = r.r[3];                /* Execution address (low 4 bytes) */
    } else {                            /* Not found, or not time-stamped  */
	tm->hi = tm->lo = 0;
    }
    *sz = (Long)(r.r[0] == 1 ? r.r[4] : 0);
#endif
}

#if HAVE_GETFINFO               /* Mac971031 */
/* --------------------------------------------------------------------------
 * Define a MacOS version of access():
 *   If the file is not accessible, -1 is returned and errno is set to
 * the reason for the failure.
 *   If the file is accessible and the dummy is 0 (existence), 2 (write), 
 * or 4 (read), the return is 0.
 *   If the file is accessible, and the dummy is 1 (executable), then if
 * the file is a program (of type 'APPL'), the return is 0, otherwise -1.
 *   Warnings: Use with caution. UNIX access do no translate to Macs.
 * Check of write access is not implemented (same as read).
 * ------------------------------------------------------------------------*/

int access(char *fileName, int dummy);

int access(char *fileName, int dummy) { 
	FInfo   fi;
	short   rc;
	
	errno = getfinfo(fileName, 0, &fi);
	if (errno != 0)  return -1;             /* Check file accessible. */
	
	/* Cases dummy = existence, read, write. */
	if (dummy == 0 || dummy & 0x6)  return 0;
	
	/* Case dummy = executable. */
	if (dummy == 1) { 
		if (fi.fdType == 'APPL')  return 0;
		errno = fi.fdType;
		return -1;
	}
	
	return 0;
}
#endif

Bool readable(f,isReg)        /* is f readable (and also, a regular file?) */
String f;
Bool   isReg; {
#if DJGPP2 || HAVE_GETFINFO /* stat returns bogus mode bits on djgpp2 */
    return (0 == access(f,4));
#elif HAVE_SYS_STAT_H || HAVE_STAT_H
    struct stat scbuf;
    return (  !stat(f,&scbuf) 
#if !(defined macintosh)	/* Macintosh files always have read permission */
	   && (scbuf.st_mode & S_IREAD) /* readable     */
#endif
	   && ( !isReg || (scbuf.st_mode & S_IFREG)) /* regular file */
	   );
#elif HAVE_OS_SWI /* RISCOS specific */
    os_regset r;                        /* RISCOS PRM p.850     -- JBS     */
    assert(dummy == 0);
    r.r[0] = 17; /* Read catalogue, no path */
    r.r[1] = (int)f;
    os_swi(OS_File, &r);
    return r.r[0] != 1; /* Does this check it's a regular file? ADR */
#endif
}


/* --------------------------------------------------------------------------
 * Search for script files on the HUGS path:
 * ------------------------------------------------------------------------*/

static String local homeDir	  Args((void));

#if __MWERKS__ && macintosh
static String local currentDir	  Args((void));
#endif

#if HSCRIPT
static String local hscriptDir    Args((Void));
static void   local hscriptSuffixes Args((Void));
#endif

#if 0
/* UNUSED */
static int    local pathCmp       Args((String, String));
#endif
static String local normPath      Args((String));
static Void   local searchChr     Args((Int));
static Void   local searchStr     Args((String));
static Bool   local tryEndings    Args((Void));
static Bool   local find1	  Args((String));
static Bool   local find2	  Args((String));
static String local expandVariable Args((String));
static String local skipVariable  Args((String));
static String local nextPath      Args((String));
static Bool   local samePath      Args((String,String));
#if DOS_FILENAMES
static Bool   local isPathSep     Args((String));
#endif
static Bool   local scanSubDirs   Args((String));

#if __MWERKS__ && macintosh
typedef char FileName[FILENAME_MAX + 1];
FileName macHugsDir; /* Directory where Hugs was found. */
#endif

#if DOS_FILENAMES
# define SLASH                   '\\'
# define isSLASH(c)              ((c)=='\\' || (c)=='/')
# define PATHSEP                 ';'
# define isPATHSEP(x)            isPathSep(x)
# define DLL_ENDING              ".dll"
#elif MAC_FILENAMES
# define SLASH                   ':'
# define isSLASH(c)              ((c)==SLASH)
# define PATHSEP                 ';'
# define isPATHSEP(x)            (*(x) == PATHSEP)
/* Mac PEF (Preferred Executable Format) file */
# define DLL_ENDING              ".pef" 
#else
# define SLASH                   '/'
# define isSLASH(c)              ((c)==SLASH)
# define PATHSEP                 ':'
# define isPATHSEP(x)            (*(x) == PATHSEP)
# define DLL_ENDING              ".so"
#endif

#if HAVE_GETMODULEFILENAME && !DOS && !cygwin32_HOST_OS
static HMODULE hugsModule = (HMODULE)0;
static String  hugsRoot   = 0;

extern Void setHugsModule Args((HMODULE));
extern Bool setHugsRoot   Args((String));

Void setHugsModule(hmod)
HMODULE hmod; {
    hugsModule = hmod;
}

Bool setHugsRoot(s)
String s; {
    String newRoot = malloc(strlen(s) + 1);
    
    if (!newRoot) return FALSE;
    
    strcpy(newRoot,s);
    
    if (hugsRoot) free(hugsRoot);
    hugsRoot = newRoot;
    return TRUE;
}

#endif


String hugsdir() {		/* directory containing libraries/Prelude.hs */
#if HSCRIPT
    /* In HaskellScript (Win32 only), we lookup InstallDir in the registry. */
    static char dir[FILENAME_MAX+1] = "";
    if (dir[0] == '\0') { /* not initialised yet */
	String s = readRegString(HKEY_LOCAL_MACHINE,hugsRegRoot,"InstallDir", 
				 HUGSDIR);
	if (s) { 
	  /* Protect against overruns */
	  strncpy(dir,s,FILENAME_MAX);
	  dir[sizeof(dir)-1] = '\0';
	  free(s);
	}
    }
    return dir;
#elif __MWERKS__ && macintosh
    static FileName dir = "\0"; /* Directory containing lib: Prelude.hs */
    strcpy(dir,macHugsDir);
    return dir;
#elif HAVE_GETMODULEFILENAME && !DOS && !cygwin32_HOST_OS
    /* On Windows, we can find the binary we're running and it's
     * conventional to put the libraries in the same place.
     */
    static char dir[FILENAME_MAX+1] = "";
   
    if (hugsRoot)
	return hugsRoot;

    if ( dir[0] == '\0' ) { /* not initialised yet */
 	String slash = NIL;
	char *hugsdir = getenv("HUGSDIR");
	
	if (hugsdir) {
	    strncpy(dir,hugsdir,FILENAME_MAX);
	} else {
	    GetModuleFileName(hugsModule,dir,FILENAME_MAX+1);
	    if ( dir[0] == '\0' ) { /* GetModuleFileName must have failed */
		return HUGSDIR;
	    }
	    if ( (slash = strrchr(dir,SLASH)) != NULL ) {
		/* truncate after directory name */
		*slash = '\0';
	    }
	}
    }
    return dir;
#else
    /* On Unix systems, data is not typically stored relative to a binary
     * (it's also harder for a binary to determine where it lives).
     * First, check for an environment var, then fall back to
     * a configuration-time constant (--datadir=...).
     */
    char *hugsdir = getenv("HUGSDIR");
    return hugsdir ? hugsdir : HUGSDIR;
#endif
}

static String homeDir() {
    return getenv("HOME");
}

#if __MWERKS__ && macintosh
static String currentDir() {
    static FileName dir = "\0";
    getcwd(dir, FILENAME_MAX);
    dir[strlen(dir) - 1] = '\0';
    return dir;
}
#endif

#if HSCRIPT    
static String local hscriptDir() {  /* Directory containing hscript.dll	   */
    static char dir[FILENAME_MAX+1] = "";
    if (dir[0] == '\0') { /* not initialised yet */
	String s = readRegString(HKEY_LOCAL_MACHINE,HScriptRoot,"InstallDir","");
	if (s) {
	  /* Protect against overruns */
	  strncpy(dir,s,FILENAME_MAX);
	  free(s);
	}
    }
    return dir;
}

static void hscriptSuffixes() {
    String ss = hugsSuffixes;
    hugsSuffixes = substPath(":.hsx:.hash",hugsSuffixes);
    free(ss);
}
#endif

String local RealPath(s)         /* Find absolute pathname of file  */
String s; {
#if HAVE__FULLPATH  /* eg DOS */
    static char path[FILENAME_MAX+1];
    _fullpath(path,s,FILENAME_MAX+1);
#elif HAVE_REALPATH /* eg Unix */
    static char path[MAXPATHLEN+1];
    path[sizeof(path)-1] = '\0';
    if (strlen(s) <= (sizeof(path)-1)) {
	realpath(s,path);                
    } else {
	return s;
    }
#else
    static char path[FILENAME_MAX+1];

    path[sizeof(path)-1] = '\0';
    if (strlen(s) <= (sizeof(path)-1)) {
        strcpy(path,s);
    } else {
	return s;
    }
#endif
    return path;
}

#if 0
/* UNUSED */
static int local pathCmp(p1,p2)       /* Compare paths after normalisation */
String p1;
String p2; {
#if HAVE__FULLPATH  /* eg DOS */
    static char path1[FILENAME_MAX+1];
    static char path2[FILENAME_MAX+1];
    _fullpath(path1,p1,FILENAME_MAX+1);
    _fullpath(path2,p2,FILENAME_MAX+1);
#elif HAVE_REALPATH /* eg Unix */
    static char path1[MAXPATHLEN+1];
    static char path2[MAXPATHLEN+1];
    realpath(p1,path1);                
    realpath(p2,path2);                
#else
    static char path1[FILENAME_MAX+1];
    static char path2[FILENAME_MAX+1];
    strcpy(path1,p1);
    strcpy(path2,p2);
#endif
#if CASE_INSENSITIVE_FILENAMES
    strlwr(path1);
    strlwr(path2);
#endif
    return filenamecmp(path1,path2);
}
#endif

static String local normPath(s) /* Try, as much as possible, to normalize  */
String s; {                     /* a pathname in some appropriate manner.  */
#if PATH_CANONICALIZATION
    String path = RealPath(s);
#if CASE_INSENSITIVE_FILENAMES
    strlwr(path);                       /* and convert to lowercase        */
#endif
    return path;
#else /* ! PATH_CANONICALIZATION */
    return s;
#endif /* ! PATH_CANONICALIZATION */
}

static char   searchBuf[FILENAME_MAX+1];
static Int    searchPos;

#define searchReset(n)          searchBuf[searchPos=(n)]='\0'

static Void local searchChr(c)  /* Add single character to search buffer   */
Int c; {
    if (searchPos<FILENAME_MAX) {
	searchBuf[searchPos++] = (char)c;
	searchBuf[searchPos]   = '\0';
    }
}

static Void local searchStr(s)  /* Add string to search buffer             */
String s; {
    while (*s && searchPos<FILENAME_MAX)
	searchBuf[searchPos++] = *s++;
    searchBuf[searchPos] = '\0';
}

static Bool local tryEndings()  /* Try each of the listed endings          */
{
    Int save;
    String sp;

    save = searchPos;
    sp = hugsSuffixes;
    while (*sp) {
	for ( ; *sp && ! isPATHSEP(sp); sp++)
	    if (searchPos<FILENAME_MAX)
		searchBuf[searchPos++] = *sp;
	searchBuf[searchPos] = '\0';
#if DEBUG_SEARCH
	Printf("trying '%s'\n", searchBuf);
#endif
	if (readable(searchBuf,TRUE))
	    return TRUE;
	if (*sp)
	    sp++;
	searchReset(save);
    }
    return FALSE;
}

#if DOS_FILENAMES
static Bool local isPathSep(sep)      /* does 'sep' mark the end of a valid path? */
String sep; {
    /* ';' is always a separator */
    /* ':' is a separator iff it is not followed by a backslash */
    /* (should test for <x>:\ but that seems too difficult) */
    return *sep == ';' || *sep == ':' && *(sep+1) != SLASH;
}
#endif

/* scandir, June 98 Daan Leijen
   searches the direct subdirectories of a directory for a file
   (excluding directories that start with a dot)

   input: searchbuf contains base directory (not SLASH terminated)
	      argument name contains the module name
   output: TRUE: searchBuf contains the full filename
	   FALSE: searchBuf is garbage, file not found
*/
	  

#if HAVE_DIRENT_H

static Bool scanSubDirs(s)
String s;
{
    DIR *dir;
    struct dirent *entry;
    struct stat statb;
    int save;

    if ((dir = opendir(searchBuf)) == NULL)
	errno = 0;
    else {
	searchChr(SLASH);
	save = searchPos;
	while ((entry = readdir(dir)) != NULL)
            if (entry->d_name[0] != '.') {
		searchStr(entry->d_name);
		if (stat(searchBuf, &statb)==0 && S_ISDIR(statb.st_mode)) {
		    searchChr(SLASH);
		    if (find2(s)) {
			closedir(dir);
			return TRUE;
		    }
		}
		searchReset(save);
	    }
	closedir(dir);
    }
    return FALSE;
}

#elif HAVE_WINDOWS_H

static Bool scanSubDirs(s)
String s;
{
    struct _finddata_t findInfo;
    long handle;
    int  save;
    
    searchChr(SLASH);
    save = searchPos;
    searchStr("*.*");
    
    /* initiate the search */
    handle = _findfirst( searchBuf, &findInfo );
    if (handle==-1) { errno = 0; return FALSE; }
    
    /* search all subdirectories */
    do {
	/* if we have a valid sub directory */
	if (((findInfo.attrib & _A_SUBDIR) == _A_SUBDIR) &&
	    (findInfo.name[0] != '.')) {
	    searchReset(save);
	    searchStr(findInfo.name);
	    searchChr(SLASH);
	    if (find2(s)) {
		_findclose( handle );
		return TRUE;
	    }
	}
    } while (_findnext( handle, &findInfo ) == 0);
    
    _findclose( handle );
    return FALSE;
}

#elif __MWERKS__ && macintosh  /* Macintosh subscan */

#include <Files.h>
#include "MoreFilesExtras.h"
#include <Errors.h>

#define MAXSPECS 50

extern StringPtr c2pstr(char *aStr);
extern char *p2cstr(StringPtr aStr);

static Bool scanSubDirs(s)
String s;
{   FileName name  = "\0";
    ConstStr255Param pname = "\p";
    String subdir = "\0";
    OSErr error;

    FSSpec specs[MAXSPECS];
    short found = 0;
    short start = 1;
    int i, save;
    
    save = searchPos;
    
    /* is it in the current directory ? */
    if (find2(s)) return TRUE;

    searchReset(save);
    
    /* initiate the search */
    
    /* the complete path to the directory is in searchBuf */
    strncpy(name,searchBuf,FILENAME_MAX);  /* do not mess up :-) */
    pname = c2pstr(name);
 
    /* get all subdirectories in path */
    error = GetDirItems( 0, 0, pname, false, true, specs
                       , MAXSPECS, &found, &start );
    
    /* search over the found directories */
    if ((error != noErr) && (error != fnfErr))
    { errno = 0;
      return FALSE;
    }
    else
    { if (found > 0)
        for (i = 0; i < found; i++)
        { subdir = p2cstr(specs[i].name);

          searchStr(subdir);
          searchChr(SLASH);

          if (find2(s))
             return TRUE;
          searchReset(save);
        }
    }

    return FALSE;
}
#else

static Bool scanSubDirs(name)
String name;
{   return FALSE;
}

#endif /* HAVE_WINDOWS_H || HAVE_DIRENT_H || (__MWERKS__ && macintosh) */

/* Variables that may be substituted in the path */

struct shellVariable {
    String var_name;
    String (*var_value) Args((Void));
};

static struct shellVariable shell_var[] = {
    { "Hugs",		&hugsdir },
    { "Home",		&homeDir },
#if __MWERKS__ && macintosh
    { "Current",	&currentDir },
#endif
#if HSCRIPT
    { "HScript",	&hscriptDir },
#endif
    { 0, 0 }
};

/*
    findPathname nm = [ nm ++ e | e <- "" : hugsSuffixes ]
*/

String findPathname(filename)   /* Look for a file, trying various extensions */
String filename; {              /* Return ***input name*** if no file was found */
    searchReset(0);
    searchStr(filename);
#if DEBUG_SEARCH
    Printf("trying '%s'\n", searchBuf);
#endif
    if (!readable(searchBuf,TRUE) && !tryEndings())
	searchStr("");
    return normPath(searchBuf);
}

/* Finding the filename corresponding to a module name:

    find maybe_dir nm = [ d ++ map dot2slash nm ++ e | d <- dirs, e <- exts ]
      where 
        dirs          = maybeToList maybe_dir ++ hugsPath
        exts          = hugsSuffixes		-- default: [".hs",".lhs"]

	-- the dir is added if the importing module was found there, or
	-- was specified as an explicit filename.

        dot2slash c   = if c=='.' then slash else c
*/

String findMInDir(dir,nm)       /* Look for a module in the suggested dir */
String dir;                     /* Return NULL if no file was found       */
String nm; {
    searchReset(0);
    searchStr(dir);
    searchChr(SLASH);
    if (find2(nm)) {
        return normPath(searchBuf);
    } else {
        return NULL;
    }
}

String findMPathname(name)	    /* Look for a module                      */
String name; {                      /* Return NULL if no file was found       */
    if (find1(name)) {
        return normPath(searchBuf);
    } else {
        return NULL;
    }
}

static Bool find1(name)		/* Search each directory of the path */
String name; {
    String pathpt = hugsPath;
    String value;

    searchReset(0);		/* look along the HUGSPATH */
    if (pathpt) {
	while (*pathpt) {
	    searchReset(0);
	    if ((value=expandVariable(pathpt)) != NULL) {
		searchStr(value);
		pathpt = skipVariable(pathpt);
	    }
	    while (*pathpt && !isPATHSEP(pathpt))
		searchChr(*pathpt++);
	    /* If the path entry ends in SLASH '*', search immediate subdirs */
	    if (searchPos >= 2 && isSLASH(pathpt[-2]) && pathpt[-1] == '*') {
		searchPos -= 2;
		searchBuf[searchPos] = '\0';
		if (scanSubDirs(name))
		    return TRUE;
	    } else {
		searchChr(SLASH);
		if (find2(name))
		    return TRUE;
	    }
	    if (isPATHSEP(pathpt))
		pathpt++;
	}
    } 
    return FALSE;    
}

/* Expansion of initial MPW-style "shell-variables" of the form {varname} */
static String local expandVariable(pathpt)
String pathpt; {
    if (*pathpt=='{') {
	int i, len;

	for (i = 0; shell_var[i].var_name!=NULL; i++) {
	    len = strlen(shell_var[i].var_name);
	    if (strncmp(pathpt+1,shell_var[i].var_name,len)==0
		&& pathpt[len+1]=='}') {
		return (*shell_var[i].var_value)();
	    }
	}
    }
    return NULL;
}

/* Assuming expandVariable(pathpt) succeeded, skip past the variable */
static String local skipVariable(pathpt)
String pathpt; {
    return strchr(pathpt+1,'}')+1;
}

static Bool local find2(s)	/* Turn module name into a filename */
String s; {
    String sp;

    /* replace all dots in the module name with slashes */
    for (sp = s; *sp; sp++) {
	searchBuf[searchPos++] = *sp == '.' ? SLASH : *sp;
    }
    return tryEndings();
}

String dirname(filename)	/* Return the directory part of the filename */
String filename; {		/* or "." if no directory.                   */
    
#if DOS_FILENAMES
    /* Allow both / and \\ as delimiters */
    /* So we cannot make use of strrchr() */
    String slash;

    slash = filename + strlen(filename) - 1;
    while (slash > filename) {
	if (isSLASH(*slash)) break;
	slash--;
    }
    if (slash <= filename) {
	return strCopy(".");
    } else {
	return strnCopy(filename, slash - filename);
    }
#else
    String slash = strrchr(filename,SLASH);
    if (!slash) {
	return strCopy(".");
    } else {
	return strnCopy(filename, slash - filename);
    }
#endif
}

/* --------------------------------------------------------------------------
 * Substitute old value of path into empty entries in new path
 * eg substPath("a:b:c::d:e","x:y:z") = "a:b:c:x:y:z:d:e"
 * ------------------------------------------------------------------------*/

String local substPath(new,sub) /* substitute sub path into new path*/
String new;
String sub; {
    Bool   substituted = FALSE;            /*   only allow one replacement */
    Int    maxlen      = strlen(sub) + strlen(new);    /* safe upper bound */
    String r = (String) malloc(maxlen+1);  /* result string                */
    String t = r;                          /* pointer into r               */
    String next = new;                     /* next uncopied char in new    */
    String start = next;                   /* start of last path component */
    if (r == 0) {
	ERRMSG(0) "String storage space exhausted"
	EEND;
    }
    do {
	if (isPATHSEP(next) || *next == '\0') {
	    if (!substituted && next == start) {
		String s = sub;
		for(; *s != '\0'; ++s) {
		    *t++ = *s;
		}
		substituted = TRUE;
	    }
	    start = next+1;
	}
    } while ((*t++ = *next++) != '\0');
    return uniqPath(r);
}

/* Remove duplicates from the path */
String uniqPath(path)
String path; {
    char *pp;
    for (pp = path; *pp; ) {
	char *prev;
	char *next = nextPath(pp);
	for (prev = path; prev != pp; prev = nextPath(prev))
	    if (samePath(prev,pp))
		break;
	if (prev == pp)		/* not found: keep entry */
	    pp = next;
	else if (*next)		/* found in middle: delete entry */
	    strcpy(pp, next);
	else {			/* found at end: delete last entry */
	    if (pp != path)
		pp--;
	    *pp = '\0';
	}
    }
    return realloc(path, strlen(path)+1);
}

/* Advance to the start of the next entry in the path list */
static String local nextPath(pp)
String pp; {
    while (*pp && !isPATHSEP(pp))
	pp++;
    if (*pp)
	pp++;
    return pp;
}

static Bool local samePath(pp1, pp2)
String pp1, pp2; {
    char *ppsave1, *ppsave2;
    char *value;

    /* initial substitution variable? */
    if ((value=expandVariable(pp1)) != NULL) {
	ppsave1 = skipVariable(pp1);
	pp1 = value;
    } else
	ppsave1 = 0;
    if ((value=expandVariable(pp2)) != NULL) {
	ppsave2 = skipVariable(pp2);
	pp2 = value;
    } else
	ppsave2 = 0;

    while (*pp1 && !isPATHSEP(pp1) && *pp2 && !isPATHSEP(pp2)) {
	if (*pp1 != *pp2)
	    return FALSE;
	if (*++pp1 == '\0' && ppsave1 != 0) {	/* end of substitution */
	    pp1 = ppsave1;
	    ppsave1 = 0;
	}
	if (*++pp2 == '\0' && ppsave2 != 0) {	/* end of substitution */
	    pp2 = ppsave2;
	    ppsave2 = 0;
	}
    }
    return (*pp1=='\0' || isPATHSEP(pp1)) && (*pp2=='\0' || isPATHSEP(pp2));
}

/* --------------------------------------------------------------------------
 * Read value from environment variable or registry:
 * ------------------------------------------------------------------------*/

String fromEnv(var,def)         /* return value of:                        */
String var;                     /*     environment variable named by var   */
String def; {                   /* or: default value given by def          */
    String s = getenv(var);     
    return (s ? s : def);
}

/* --------------------------------------------------------------------------
 * Get time/date stamp for inclusion in compiled files:
 * ------------------------------------------------------------------------*/

#if PROFILING
String timeString() {                   /* return time&date string         */
    time_t clock;                       /* must end with '\n' character    */
    time(&clock);
    return(ctime(&clock));
}
#endif

/* --------------------------------------------------------------------------
 * Garbage collection notification:
 * ------------------------------------------------------------------------*/

Bool gcMessages = FALSE;                /* TRUE => print GC messages       */

Void gcStarted() {                      /* Notify garbage collector start  */
    if (gcMessages) {
	Printf("{{Gc");
	FlushStdout();
    }
}

Void gcScanning() {                     /* Notify garbage collector scans  */
    if (gcMessages) {
	Putchar(':');
	FlushStdout();
    }
}

Void gcRecovered(recovered)		/* Notify garbage collection done  */
Int recovered; {
    if (gcMessages) {
	Printf("%d}}",recovered);
	FlushStdout();
    }
}

Cell *CStackBase;                       /* Retain start of C control stack */

#if RISCOS                              /* Stack traversal for RISCOS      */

/* Warning: The following code is specific to the Acorn ARM under RISCOS
   (and C4).  We must explicitly walk back through the stack frames, since
   the stack is extended from the heap. (see PRM pp. 1757).  gcCStack must
   not be modified, since the offset '5' assumes that only v1 is used inside
   this function. Hence we do all the real work in gcARM.
*/
		  
#define spreg 13 /* C3 has SP=R13 */

#define previousFrame(fp)       ((int *)((fp)[-3]))
#define programCounter(fp)      ((int *)((*(fp)-12) & ~0xFC000003))
#define isSubSPSP(w)            (((w)&dontCare) == doCare)
#define doCare                  (0xE24DD000)  /* SUB r13,r13,#0 */
#define dontCare                (~0x00100FFF) /* S and # bits   */
#define immediateArg(x)         ( ((x)&0xFF) << (((x)&0xF00)>>7) )

static void gcARM(int *fp) {
    int si = *programCounter(fp);       /* Save instruction indicates how */
					/* many registers in this frame   */
    int *regs = fp - 4;
    if (si & (1<<0)) markWithoutMove(*regs--);
    if (si & (1<<1)) markWithoutMove(*regs--);
    if (si & (1<<2)) markWithoutMove(*regs--);
    if (si & (1<<3)) markWithoutMove(*regs--);
    if (si & (1<<4)) markWithoutMove(*regs--);
    if (si & (1<<5)) markWithoutMove(*regs--);
    if (si & (1<<6)) markWithoutMove(*regs--);
    if (si & (1<<7)) markWithoutMove(*regs--);
    if (si & (1<<8)) markWithoutMove(*regs--);
    if (si & (1<<9)) markWithoutMove(*regs--);
    if (previousFrame(fp)) {
	/* The non-register stack space is for the previous frame is above
	   this fp, and not below the previous fp, because of the way stack
	   extension works. It seems the only way of discovering its size is
	   finding the SUB sp, sp, #? instruction by walking through the code
	   following the entry point.
	*/
	int *oldpc = programCounter(previousFrame(fp));
	int fsize = 0, i;
	for(i = 1; i < 6; ++i)
	    if(isSubSPSP(oldpc[i])) fsize += immediateArg(oldpc[i]) / 4;
	for(i=1; i<=fsize; ++i)
	    markWithoutMove(fp[i]);
    }
}

void gcCStack() {
    int dummy;
    int *fp = 5 + &dummy;
    while (fp) {
	gcARM(fp);
	fp = previousFrame(fp);
    }
}

#else                   /* Garbage collection for standard stack machines  */

Void gcCStack() {                       /* Garbage collect elements off    */
    Cell stackTop = NIL;                /* C stack                         */
    Cell *ptr = &stackTop;
#if SIZEOF_INTP == 2
    if (((long)(ptr) - (long)(CStackBase))&1)
	fatal("gcCStack");
#elif STACK_ALIGNMENT == 2 /* eg Macintosh 68000 */
    if (((long)(ptr) - (long)(CStackBase))&1)
	fatal("gcCStack");
#else 
    if (((long)(ptr) - (long)(CStackBase))&3)
	fatal("gcCStack");
#endif

#define StackGrowsDown  while (ptr<=CStackBase) markWithoutMove(*ptr++)
#define StackGrowsUp    while (ptr>=CStackBase) markWithoutMove(*ptr--)
#define GuessDirection  if (ptr>CStackBase) StackGrowsUp; else StackGrowsDown

#if STACK_DIRECTION > 0
    StackGrowsUp;
#elif STACK_DIRECTION < 0
    StackGrowsDown;
#else
    GuessDirection;
#endif

#if SIZEOF_INTP==4 && STACK_ALIGNMENT == 2 /* eg Macintosh 68000 */
    ptr = (Cell *)((long)(&stackTop) + 2);
    StackGrowsDown;
#endif

#undef  StackGrowsDown
#undef  StackGrowsUp
#undef  GuessDirection
}
#endif

/* --------------------------------------------------------------------------
 * Terminal dependent stuff:
 * ------------------------------------------------------------------------*/

#if (HAVE_TERMIO_H | HAVE_SGTTY_H | HAVE_TERMIOS_H)

/* grab the varargs prototype for ioctl */
#if HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif

/* The order of these three tests is very important because
 * some systems have more than one of the requisite header file
 * but only one of them seems to work.
 * Anyone changing the order of the tests should try enabling each of the
 * three branches in turn and write down which ones work as well as which
 * OS/compiler they're using.
 *
 * OS            Compiler      sgtty     termio  termios   notes
 * Linux 2.0.18  gcc 2.7.2     absent    works   works     1
 *
 * Notes:
 * 1) On Linux, termio.h just #includes termios.h and sgtty.h is
 *    implemented using termios.h.
 *    sgtty.h is in /usr/include/bsd which is not on my standard include
 *    path.  Adding it does no harm but you might as well use termios.
 *    --
 *    alastair@reid-consulting-uk.ltd.uk
 */
#if HAVE_TERMIOS_H

#include <termios.h>
typedef  struct termios  TermParams;
#define  getTerminal(fd,tp) tcgetattr(fd, &tp)
#define  setTerminal(fd,tp) tcsetattr(fd, TCSAFLUSH, &tp)
#define  getEcho(tp)     ((tp.c_lflag | ECHO) != 0)
#define  noEcho(tp)      tp.c_lflag    &= ~ECHO;
#define  doEcho(tp)      tp.c_lflag    |= ECHO;
#define  getBuff(tp)     ((tp.c_lflag | ICANON) != 0)
#define  noBuff(tp)      tp.c_lflag    &= ~ICANON; \
			 tp.c_cc[VMIN]  = 1; \
			 tp.c_cc[VTIME] = 0;
#define  doBuff(tp)      tp.c_lflag    |= ICANON; \
			 tp.c_cc[VEOF]  = '\04'; \
			 tp.c_cc[VEOL]  = '\0';

#elif HAVE_SGTTY_H

#include <sgtty.h>
typedef  struct sgttyb   TermParams;
#define  getTerminal(fd,tp) ioctl(fd,TIOCGETP,&tp)
#define  setTerminal(fd,tp) ioctl(fd,TIOCSETP,&tp)
#define  getEcho(tp)     ((tp.sg_flags | ECHO) != 0)
#define  noEcho(tp)      tp.sg_flags &= ~ECHO;
#define  doEcho(tp)      tp.sg_flags |= ECHO;
#if HPUX
#define  getBuff(tp)     ((tp.sg_flags | RAW) == 0)
#define  noBuff(tp)      tp.sg_flags |= RAW;
#define  doBuff(tp)      tp.sg_flags &= ~RAW;
#else
#define  getBuff(tp)     ((tp.sg_flags | CBREAK) == 0)
#define  noBuff(tp)      tp.sg_flags |= CBREAK;
#define  doBuff(tp)      tp.sg_flags &= ~CBREAK;
#endif

#elif HAVE_TERMIO_H

#include <termio.h>
typedef  struct termio   TermParams;
#define  getTerminal(fd,tp) ioctl(fd,TCGETA,&tp)
#define  setTerminal(fd,tp) ioctl(fd,TCSETAF,&tp)
#define  getEcho(tp)     ((tp.c_lflag | ECHO) != 0)
#define  noEcho(tp)      tp.c_lflag    &= ~ECHO;
#define  doEcho(tp)      tp.c_lflag    |= ECHO;
#define  getBuff(tp)     ((tp.c_lflag | ICANON) != 0)
#define  noBuff(tp)      tp.c_lflag    &= ~ICANON; \
			 tp.c_cc[VMIN]  = 1; \
			 tp.c_cc[VTIME] = 0;
#define  doBuff(tp)      tp.c_lflag    |= ICANON; \
			 tp.c_cc[VEOF]  = '\04'; \
			 tp.c_cc[VEOL]  = '\0';

#endif

static Bool messedWithTerminal = FALSE;
static TermParams originalSettings;

Void normalTerminal() {                 /* restore terminal initial state  */
    if (messedWithTerminal)
	setTerminal(fileno(stdin), originalSettings);
}

Bool getEchoTerminal(Int fd) {
    TermParams settings;

    getTerminal(fd, settings);
    return getEcho(settings);
}

Void setEchoTerminal(Int fd, Bool echo) {
    TermParams settings;

    if (fd==0 && !messedWithTerminal) {
	getTerminal(0, originalSettings);
	messedWithTerminal = TRUE;
    }
    getTerminal(fd, settings);
    if (echo) {
	doEcho(settings);
    } else {
	noEcho(settings);
    }
    setTerminal(fd, settings);
}

Bool getBuffTerminal(Int fd) {
    TermParams settings;

    getTerminal(fd, settings);
    return getEcho(settings);
}

Void setBuffTerminal(Int fd, Bool buffered) {
    TermParams settings;

    if (fd==0 && !messedWithTerminal) {
	getTerminal(0, originalSettings);
	messedWithTerminal = TRUE;
    }
    getTerminal(fd, settings);
    if (buffered) {
	doBuff(settings);
    } else {
	noBuff(settings);
    }
    setTerminal(fd, settings);
}

Int getTerminalWidth() {                /* determine width of terminal     */
#ifdef TIOCGWINSZ
#ifdef _M_UNIX                          /* SCO Unix 3.2.4 defines TIOCGWINSZ*/
#include <sys/stream.h>                 /* Required by sys/ptem.h          */
#include <sys/ptem.h>                   /* Required to declare winsize     */
#endif
    static struct winsize terminalSize;
    ioctl(fileno(stdout),TIOCGWINSZ,&terminalSize);
    return (terminalSize.ws_col==0)? 80 : terminalSize.ws_col;
#else
    return 80;
#endif
}

#elif __MWERKS__ && macintosh
#include <limits.h>

Int getTerminalWidth() {
    /* Never insert extra '\n' in output, as the console softwraps. */
    return INT_MAX;
}

Void normalTerminal() {
}

Bool getEchoTerminal(Int fd) {
    return TRUE;
}

Void setEchoTerminal(Int fd, Bool echo) {
}

Bool getBuffTerminal(Int fd) {
    return TRUE;
}

Void setBuffTerminal(Int fd, Bool buffered) {
}

#elif IS_WINDOWS

static Bool messedWithTerminal = FALSE;
static DWORD originalSettings;

Int getTerminalWidth() {
    return 80;
}

Void normalTerminal() {                 /* restore terminal initial state  */
    if (messedWithTerminal) {
 	HANDLE hIn;

 	hIn = GetStdHandle(STD_INPUT_HANDLE);
 	SetConsoleMode(hIn, originalSettings);
	messedWithTerminal = FALSE;
    }
}

Bool getEchoTerminal(Int fd) {
    if (fd==0) {
 	DWORD mo;
 	HANDLE hIn;

 	hIn = GetStdHandle(STD_INPUT_HANDLE);
 	GetConsoleMode(hIn, &mo);
	return (mo & ENABLE_ECHO_INPUT)!=0;
    } else
	return FALSE;
}

Void setEchoTerminal(Int fd, Bool echo) {
    if (fd==0) {
 	DWORD mo;
 	HANDLE hIn;

 	hIn = GetStdHandle(STD_INPUT_HANDLE);
 	GetConsoleMode(hIn, &mo);
	if (!messedWithTerminal) {
	    originalSettings = mo;
	    messedWithTerminal = TRUE;
	}
	if (echo)
	    mo |= ENABLE_ECHO_INPUT;
	else
	    mo &= ~ENABLE_ECHO_INPUT;
 	SetConsoleMode(hIn, mo);
    }
}

Bool getBuffTerminal(Int fd) {
    if (fd==0) {
 	DWORD mo;
 	HANDLE hIn;

 	hIn = GetStdHandle(STD_INPUT_HANDLE);
 	GetConsoleMode(hIn, &mo);
	return (mo & ENABLE_LINE_INPUT)!=0;
    } else
	return FALSE;
}

Void setBuffTerminal(Int fd, Bool buffered) {
    if (fd==0) {
 	DWORD mo;
 	HANDLE hIn;

 	hIn = GetStdHandle(STD_INPUT_HANDLE);
 	GetConsoleMode(hIn, &mo);
	if (!messedWithTerminal) {
	    originalSettings = mo;
	    messedWithTerminal = TRUE;
	}
	if (buffered)
	    mo |= ENABLE_LINE_INPUT;
	else
	    mo &= ~ENABLE_LINE_INPUT;
 	SetConsoleMode(hIn, mo);
    }
}

#else /* no terminal driver - eg DOS, RISCOS */

Int getTerminalWidth() {
#if RISCOS
    int dummy, width;
    (void) os_swi3r(OS_ReadModeVariable, -1, 1, 0, &dummy, &dummy, &width);
    return width+1;
#else
    return 80;
#endif
}

Void normalTerminal() {                 /* restore terminal initial state  */
}

Bool getEchoTerminal(Int fd) {
    return fd==0;
}

Void setEchoTerminal(Int fd, Bool echo) {
}

Bool getBuffTerminal(Int fd) {
    return fd==0;
}

Void setBuffTerminal(Int fd, Bool buffered) {
}

#if 0
Int readTerminalChar() {                /* read character from terminal    */
    if (terminalEchoReqd) {
	return getchar();
    } else {
#if IS_WINDOWS && !HUGS_FOR_WINDOWS && !__BORLANDC__
	/* When reading a character from the console/terminal, we want
	 * to operate in 'raw' mode (to use old UNIX tty parlance) and have
 	 * it return when a character is available and _not_ wait until
 	 * the next time the user hits carriage return. On Windows platforms,
 	 * this _can_ be done by reading directly from the console, using
	 * getch().  However, this doesn't sit well with programming
	 * environments such as Emacs which allow you to create sub-processes
	 * running Hugs, and then communicate with the running interpreter
	 * through its standard input and output handles. If you use getch()
	 * in that setting, you end up trying to read the (unused) console
	 * of the editor itself, through which not a lot of characters is
	 * bound to come out, since the editor communicates input to Hugs
	 * via the standard input handle.
 	 *
 	 * To avoid this rather unfortunate situation, we use the Win32
	 * console API and re-jig the input properties of the standard
	 * input handle before trying to read a character using stdio's
	 * getchar().
 	 * 
 	 * The 'cost' of this solution is that it is Win32 specific and
	 * won't work with Windows 3.1 + it is kind of ugly and verbose
	 * to have to futz around with the console properties on a
	 * per-char basis. Both of these disadvantages aren't in my
	 * opinion fatal.
 	 *
 	 * -- sof 5/99
 	 */
        Int c;
 	DWORD mo;
 	HANDLE hIn;
	static int isEmacs = -1;
 
 	/* Cannot claim to fully understand, but if the FILE*s underlying
	   file descriptor is in text mode, we seem to lose the first
	   carriage return.
 	 */
 	setmode(fileno(stdin), _O_BINARY);
 	hIn = GetStdHandle(STD_INPUT_HANDLE);
 	GetConsoleMode(hIn, &mo);
 	SetConsoleMode(hIn, mo & ~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT));
	/* 
	 * When using the read-eval-print loop inside of a Win32 console, a
	 * lone '\n' is returned by getc() after switching to binary mode.
	 * Since Enter maps to a raw '\r', and we map this (below) to '\n',
	 * we can just ignore all *raw* '\n's.
	 *
	 * However, Emacs subshells (via comint) doesn't emit '\r's, just \n's,
	 * which is incompatible with the above. The hack/workaround, is to
	 * dynamically check whether we're exec'ing within Emacs and fall
	 * back to the simple, non-\n stripping input mode if we are. sigh.
	 * 
	 */
	if (isEmacs < 0) {
	  isEmacs = (getenv("EMACS") != NULL);
	}

	if (isEmacs) {
	  c = getc(stdin);
	} else {
	  do {
	    c = getc(stdin);
	  } while (c == '\n');
	}

 	/* Same as it ever was - revert back state of stdin. */
 	SetConsoleMode(hIn, mo);
	setmode(fileno(stdin), _O_TEXT);
#else
	Int c = getch();
#endif
	return c=='\r' ? '\n' : c;      /* slight paranoia about CR-LF    */
    }
}
#endif

#endif /* no terminal driver */

/* --------------------------------------------------------------------------
 * Interrupt handling:
 * ------------------------------------------------------------------------*/

Bool    broken         = FALSE;		/* pending break to be handled     */
static  Bool breakReqd = FALSE;		/* currently trapping breaks       */
static  Bool trapBreak = FALSE;		/* ever asked to trap breaks       */
static  sigProto(ignoreBreak);
static  Void local installHandlers Args((Void));

Bool breakOn(reqd)                      /* set break trapping on if reqd,  */
Bool reqd; {                            /* or off otherwise, returning old */
    Bool old  = breakReqd;

    breakReqd = reqd;
    if (reqd) {
	trapBreak = TRUE;
	if (broken) {                   /* repond to break signal received */
	    broken = FALSE;             /* whilst break trap disabled      */
	    sigRaise(breakHandler);
	    /* not reached */
	}
#if HANDLERS_CANT_LONGJMP
	ctrlbrk(ignoreBreak);
#else
	ctrlbrk(breakHandler);
#endif
    } else if (trapBreak) {		/* If we have been trapping breaks, */
	ctrlbrk(ignoreBreak);		/* switch to deferring them.        */
    }
    return old;
}

static sigHandler(ignoreBreak) {        /* record but don't respond to break*/
    ctrlbrk(ignoreBreak);         /* reinstall signal handler               */
				  /* redundant on BSD systems but essential */
				  /* on POSIX and other systems             */
    broken = TRUE;
    sigResume;
}

#if !DONT_PANIC
static sigProto(panic);
static sigHandler(panic) {              /* exit in a panic, on receipt of  */
    everybody(EXIT);                    /* an unexpected signal            */
    fprintf(stderr,"\nUnexpected signal\n");
    exit(1);
    sigResume;/*NOTREACHED*/
}
#endif /* !DONT_PANIC */

#if IS_WINDOWS
BOOL WINAPI consoleHandler(DWORD dwCtrlType) {
    switch (dwCtrlType) {		/* Allows Hugs to be terminated    */
	case CTRL_CLOSE_EVENT :		/* from the window's close menu.   */
	    ExitProcess(0);
    }
    return FALSE;
}
#endif

static Void local installHandlers() { /* Install handlers for all fatal    */ 
				      /* signals except SIGINT and SIGBREAK*/
#if IS_WINDOWS
    SetConsoleCtrlHandler(consoleHandler,TRUE);
#endif
#if !DONT_PANIC && !DOS
# ifdef SIGABRT
    signal(SIGABRT,panic);
# endif
# ifdef SIGBUS
    signal(SIGBUS,panic);
# endif
# ifdef SIGFPE
    signal(SIGFPE,panic);
# endif
# ifdef SIGHUP
    signal(SIGHUP,panic);
# endif
# ifdef SIGILL
    signal(SIGILL,panic);
# endif
# ifdef SIGQUIT
    signal(SIGQUIT,panic);
# endif
# ifdef SIGSEGV
    signal(SIGSEGV,panic);
# endif
# ifdef SIGTERM
    signal(SIGTERM,panic);
# endif
#endif /* !DONT_PANIC && !DOS */

}

/* --------------------------------------------------------------------------
 * Shell escapes:
 * ------------------------------------------------------------------------*/

Int
shellEsc(cmd, sync, useShell)         /* run a shell command (or shell)  */
String cmd;
Bool   sync;
Bool   useShell; {
#ifndef HAVE_WINDOWS_H

  /* currently ignore the 'useShell' and 'sync' flags */
# if HAVE_MACSYSTEM
    return macsystem(cmd);
# else
#  if HAVE_BIN_SH
    if (cmd[0]=='\0') {
	cmd = fromEnv("SHELL","/bin/sh");
    }
# endif
    return system(cmd);
# endif
#else
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  BOOL bStatus;
  DWORD dwResult;

  if (useShell) {
    return system(cmd);
  }

  ZeroMemory(&si, sizeof(si));
  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESHOWWINDOW;
  si.wShowWindow = SW_SHOW;
  
  bStatus = 
    CreateProcess(NULL,  /* app name is the first component of the command line string */
		  cmd, 
		  NULL,  /* default process security attributes */
		  NULL,  /* default prim. thread security attributes */
		  FALSE, /* don't inherit */
		  CREATE_NEW_CONSOLE,
		  NULL,  /* environment; same block as parent */
		  NULL,  /* current directory; same as parent */
		  &si,
		  &pi);
		    
  if (!bStatus) {
    return 1;
  } else {
    CloseHandle(pi.hThread);
    /*
     * Wait for the editor process to complete, or not. If we
     * don't wait for the editor process to complete, the user
     * will have to manually :(re)load the sources after having
     * save them within the editor. The default is to wait.
     */
    if (!sync) {
      CloseHandle(pi.hProcess);
      return 0;
    } else {
# if !HUGS_FOR_WINDOWS
      dwResult = WaitForSingleObject(pi.hProcess, INFINITE);
      return (dwResult == WAIT_OBJECT_0 ? 0 : 1);
# else
      MSG msg;

      while (1) {
	dwResult = MsgWaitForMultipleObjects(1, 
					     &pi.hProcess,
					     FALSE, /* fWaitAll */
					     INFINITE,
					     QS_PAINT);
	if (dwResult == WAIT_OBJECT_0) {
	  return 0;
	} else if (dwResult == (WAIT_OBJECT_0 + 1)) {
	  /* Dispatch waiting messages. */
	  while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
	    TranslateMessage(&msg);
	    DispatchMessage(&msg);
	  }
	} else {
	  return 1;
	}
      }
# endif
    }
  }
#endif
}

#if RISCOS                              /* RISCOS also needs a chdir()     */
int chdir(char *s) {                    /* RISCOS PRM p. 885    -- JBS     */
    return os_swi2(OS_FSControl + XOS_Bit, 0, (int)s) != NULL;
}
#endif


/* --------------------------------------------------------------------------
 * Floating point support:
 * ------------------------------------------------------------------------*/

#if FLOATS_SUPPORTED
#if BREAK_FLOATS
static union {
    Float  flVal;
    struct {
	Cell flPart1,flPart2;
    }      clVal;
} fudgeCoerce;

Cell bfTemp = NIL;

Cell mkFloat(fl)
FloatPro fl; {
    Cell p1,p2;
    fudgeCoerce.flVal = fl;
    bfTemp = mkInt(fudgeCoerce.clVal.flPart1);
    p2     = mkInt(fudgeCoerce.clVal.flPart2);
    p1     = bfTemp;
    bfTemp = NIL;
    return pair(FLOATCELL,pair(p1,p2));
}

FloatPro floatOf(c)
Cell c; {
    fudgeCoerce.clVal.flPart1 = intOf(fst(snd(c)));
    fudgeCoerce.clVal.flPart2 = intOf(snd(snd(c)));
    return fudgeCoerce.flVal;
}

#else /* !BREAK_FLOATS */
static union {
    Float flVal;
    Cell  clVal;
} fudgeCoerce;

Cell mkFloat(fl)
FloatPro fl; {
    fudgeCoerce.flVal = (Float)fl;
    return pair(FLOATCELL,fudgeCoerce.clVal);
}

FloatPro floatOf(c)
Cell c; {
    fudgeCoerce.clVal = snd(c);
    return fudgeCoerce.flVal;
}
#endif /* !BREAK_FLOATS */

String floatToString(fl)                     /* Make sure that floating    */
FloatPro fl; {                               /* point values print out in  */
    static char buffer1[32];                 /* a form in which they could */
    static char buffer2[32];                 /* also be entered as floats  */
    Int i=0, j=0;

    sprintf(buffer1,FloatFMT,fl);
    while (buffer1[i] && strchr("eE.",buffer1[i])==0)
	buffer2[j++] = buffer1[i++];
    if (buffer1[i]=='\0') {
        sprintf(buffer1,"%.1f",fl);
        i = j = 0;
    } else if (buffer1[i]!='.') {
	buffer2[j++] = '.';
	buffer2[j++] = '0';
    }
    while ((buffer2[j++]=buffer1[i++])!=0) {
    }
    return buffer2;
}

static union {
    Double  dblVal;
    struct {
	Cell dblPart1,dblPart2;
    }      cdVal;
} fudgeDCoerce;

Cell part1Double(dbl)
DoublePro dbl; {
    fudgeDCoerce.dblVal = dbl;
    return fudgeDCoerce.cdVal.dblPart1;
}

Cell part2Double(dbl)
DoublePro dbl; {
    fudgeDCoerce.dblVal = dbl;
    return fudgeDCoerce.cdVal.dblPart2;
}

DoublePro doubleFromParts(c1,c2)
Cell c1, c2; {
    fudgeDCoerce.cdVal.dblPart1 = c1;
    fudgeDCoerce.cdVal.dblPart2 = c2;
    return fudgeDCoerce.dblVal;
}

Cell bdTemp = NIL;

Cell mkDouble(dbl)
DoublePro dbl; {
    Cell p1,p2;
    fudgeDCoerce.dblVal = dbl;
    bdTemp = mkInt(fudgeDCoerce.cdVal.dblPart1);
    p2     = mkInt(fudgeDCoerce.cdVal.dblPart2);
    p1     = bdTemp;
    bdTemp = NIL;
    return pair(DOUBLECELL,pair(p1,p2));
}

DoublePro doubleOf(c)
Cell c; {
    fudgeDCoerce.cdVal.dblPart1 = intOf(fst(snd(c)));
    fudgeDCoerce.cdVal.dblPart2 = intOf(snd(snd(c)));
    return fudgeDCoerce.dblVal;
}

String doubleToString(dbl)                   /* Make sure that floating    */
DoublePro dbl; {                             /* point values print out in  */
    static char buffer1[32];                 /* a form in which they could */
    static char buffer2[32];                 /* also be entered as doubles */
    Int i=0, j=0;

    sprintf(buffer1,DoubleFMT,dbl);
    while (buffer1[i] && strchr("eE.",buffer1[i])==0)
	buffer2[j++] = buffer1[i++];
    if (buffer1[i]=='\0') {
        sprintf(buffer1,"%.1f",dbl);
        i = j = 0;
    } else if (buffer1[i]!='.') {
	buffer2[j++] = '.';
	buffer2[j++] = '0';
    }
    while ((buffer2[j++]=buffer1[i++])!=0) {
    }
    return buffer2;
}

DoublePro stringToDouble(s)
String s; {
    return atof(s);
}
#else /* !FLOATS_SUPPORTED */
Cell mkFloat(fl)
FloatPro fl; {
    internal("mkFloat");
    return 0;/*NOTREACHED*/
}

FloatPro floatOf(c)
Cell c; {
    internal("floatOf");
    return 0;/*NOTREACHED*/
}

String floatToString(fl)
FloatPro fl; {
    internal("floatToString");
    return "";/*NOTREACHED*/
}

Cell part1Double(dbl)
DoublePro dbl; {
    internal("part1Double");
    return 0;/*NOTREACHED*/
}

Cell part2Double(dbl)
DoublePro dbl; {
    internal("part2Double");
    return 0;/*NOTREACHED*/
}

DoublePro doubleFromParts(c1,c2)
Cell c1, c2; {
    internal("doubleFromParts");
    return 0;/*NOTREACHED*/
}

Cell mkDouble(fl)
DoublePro fl; {
    internal("mkDouble");
    return 0;/*NOTREACHED*/
}

DoublePro doubleOf(c)
Cell c; {
    internal("doubleOf");
    return 0;/*NOTREACHED*/
}

String doubleToString(fl)
DoublePro fl; {
    internal("doubleToString");
    return "";/*NOTREACHED*/
}
#endif /* !FLOATS_SUPPORTED */

/*---------------------------------------------------------------------------
 * Int64-related operations:
 *-------------------------------------------------------------------------*/

#if PROVIDE_INT64
Int part1Int64(i)
HsInt64 i; {
    return (Int)(i >> 32);
}

Int part2Int64(i)
HsInt64 i; {
    return (Int)(i);
}

HsInt64 int64FromParts(c1,c2)
Int c1, c2; {
    return ((HsInt64)c1 << 32) | (HsInt64)((HsWord32)c2);
}
#endif /* PROVIDE_INT64 */

/*---------------------------------------------------------------------------
 * Printf-related operations:
 *-------------------------------------------------------------------------*/

#if !HAVE_VSNPRINTF
int vsnprintf(char* buffer, size_t count, const char* fmt, va_list ap);
int vsnprintf(char* buffer, size_t count, const char* fmt, va_list ap) {
#if HAVE__VSNPRINTF
    return _vsnprintf(buffer, count, fmt, ap);
#else
    return 0;
#endif
}
#endif /* HAVE_VSNPRINTF */

#if !HAVE_SNPRINTF && !HAVE__SNPRINTF
int snprintf(char* buffer, size_t count, const char* fmt, ...);
int snprintf(char* buffer, size_t count, const char* fmt, ...) {
#if HAVE_VSNPRINTF || HAVE__VSNPRINTF
    int r;
    va_list ap;                    /* pointer into argument list           */
    va_start(ap, fmt);             /* make ap point to first arg after fmt */
    r = vsnprintf(buffer, count, fmt, ap);
    va_end(ap);                    /* clean up                             */
    return r;
#else
    return 0;
#endif
}
#endif /* HAVE_SNPRINTF */

/* --------------------------------------------------------------------------
 * Dynamic loading:
 * ------------------------------------------------------------------------*/

static void* local getModDLL Args((String));
static void* local getDLL Args((String));
static void* local getDLLSymbol Args((void*,String));

#if HAVE_DLFCN_H /* eg LINUX, SOLARIS, ULTRIX */

#include <stdio.h>
#include <dlfcn.h>

static void* local getDLL(dll)  /* load dll */
String dll; {
    void *instance = dlopen(dll,
			    0
#if defined(RTLD_LAZY) /* eg SunOS4 doesn't have RTLD_NOW */
			    | RTLD_LAZY 
# if defined(RTLD_GLOBAL)
			    | RTLD_GLOBAL
# endif
#elif defined(RTLD_NOW)
			    | RTLD_NOW
#else /* eg FreeBSD doesn't have RTLD_LAZY */
			    | 1
#endif
			    );

    if (NULL == instance) {
	ERRMSG(0) "Error while importing DLL \"%s\":\n%s\n", dll, dlerror()
	EEND;
    }
    
    return instance;
}

static void* local getDLLSymbol(instance,symbol)  /* lookup dll symbol */
void*  instance;
String symbol; {
    void *sym;

    if ((sym = dlsym(instance,symbol)) == 0) {
	ERRMSG(0) "Error loading sym:\n%s\n", dlerror()
	EEND;
    }

    return sym;
}

void freeDLL (dll) /* free up DLL */
void* dll; {
  if (dll) {
    /* No error checking done. */
    dlclose(dll);
  }
  return;
}

#elif HAVE_DL_H /* eg HPUX */

#include <dl.h>

static void* local getDLL(dll)  /* load dll */
String dll; {
    shl_t instance = shl_load(dll,BIND_IMMEDIATE,0L);

    if (NULL == instance) {
	ERRMSG(0) "Error while importing DLL \"%s\"", dll
	EEND;
    }
    /* Assuming that shl_t can be converted into a void* with
       loss of information here... is this OK? */
    return instance;
}
static void* local getDLLSymbol(dll,symbol)  /* lookup dll symbol */
void* dll;
String symbol; {
    void* r;
    return (0 == shl_findsym(&(shl_t)dll,symbol,TYPE_PROCEDURE,&r)) ? r : 0;
}

Void freeDLL(dll)
void* dll; {
  if (dll) {
    shl_unload((shl_t)dll);
  }
  return;
}

#elif HAVE_WINDOWS_H && !defined(__MSDOS__)

static void* local getDLL(dll)  /* load dll */
String dll; {
    HINSTANCE instance = LoadLibrary(dll);
    if (NULL == instance) {
	/* GetLastError allegedly provides more detail - in practice,
	 * it tells you nothing more.
	 */
	ERRMSG(0) "Error while importing DLL \"%s\"", dll
	EEND;
    }
    /* fprintf(stderr, "Loaded DLL 0x%p\n",instance); fflush(stderr); */
    return instance;
}

static void* local getDLLSymbol(instance,symbol)  /* lookup dll symbol */
void* instance;
String symbol; {
    return (void*)GetProcAddress((HINSTANCE)instance,symbol);
}

Void freeDLL(dll)
void* dll; {
  if (dll) {
    /* fprintf(stderr, "Freeing DLL 0x%p\n",dll); fflush(stderr); */
    FreeLibrary(dll);
  }
  return;
}


#elif HAVE_MACH_O_DYLD_H         /* MacOS X */

/*****************************************************************************/

#include <stdio.h>
#include <mach-o/dyld.h>

/* static char* dl_last_error = ( char* ) 0; */

static int dlerror_index = 1;

static char* dlerror(  ) {
  static char* OFIErrorStrings[] = {
    "Object Image Load Failure\n",
    "Object Image Load Success\n",
    "Not an recognisable object file\n",
    "No valid architecture\n",
    "Object image has an invalid format\n",
    "Invalid access (permissions?)\n",
    "Unknown error code from NSCreateObjectFileImageFromFile\n",
  };

#define NUM_OFI_ERRORS ( sizeof( OFIErrorStrings ) /\
                         sizeof( OFIErrorStrings[ 0 ] ) )

  if( dlerror_index > NUM_OFI_ERRORS - 1 )
    dlerror_index = NUM_OFI_ERRORS - 1;

  return OFIErrorStrings[ dlerror_index ];
}

int dlclose( void* handle ) {
  NSUnLinkModule( handle, FALSE );
  return 0;
}

static void* dlopen( char* path, int mode /* mode is ignored */ ) {
  int dyld_result;
  NSObjectFileImage ofile;
  NSModule handle = NULL;

  dyld_result = NSCreateObjectFileImageFromFile( path, &ofile );
  if( dyld_result != NSObjectFileImageSuccess )
    dlerror_index = dyld_result;
  else {
    handle = NSLinkModule( ofile, path, NSLINKMODULE_OPTION_PRIVATE );
  }

  return handle;
}

void* dlsym( void* handle, char* symbol ) {
  void* addr;

  NSSymbol s = NSLookupSymbolInModule( (NSModule)handle, symbol );
  if( s ) {
    addr = NSAddressOfSymbol(s);
  } else {
    addr = NULL;
  }
  return addr;
}

/*****************************************************************************/

static void* local getDLL(dll)  /* load dll */
String dll; {
  void *instance = dlopen(dll,1);
  if (NULL == instance) {
    ERRMSG(0) "Error while importing DLL \"%s\":\n%s\n", dll, dlerror()
      EEND;
  }
  return instance;
}

static void* local getDLLSymbol(instance,symbol)  /* lookup dll symbol */
void* instance;
String symbol; {
  void *sym;
  
  if (sym = dlsym(instance,symbol))
    return sym;
  
  ERRMSG(0) "Error loading sym: %s\n", symbol
    EEND;
}

Void freeDLL(dll)
void* dll; {
  if (dll) {
    dlclose(dll);
  }
  return;
}

#else /* Dynamic loading not available */

static void* local getDLL(dll)  /* load dll */
String dll; {
#if 1 /* very little to choose between these options */
    return 0;
#else
    ERRMSG(0) "This Hugs build does not support plugins\n"
    EEND;
#endif
}
static void* local getDLLSymbol(dll,symbol)  /* load dll and lookup symbol */
void* dll;
String symbol; {
#if 1 /* very little to choose between these options */
    return 0;
#else
    ERRMSG(0) "This Hugs build does not support plugins\n"
    EEND;
#endif
}

Void freeDLL(dll)
void* dll; {
}

#endif /* Dynamic loading not available */

static void* local getModDLL(file)		/* load DLL for module     */
String file; {
#if HAVE_REALPATH && !HAVE__FULLPATH
    char dllPath[MAXPATHLEN+1];
#else
    char dllPath[FILENAME_MAX+1];
#endif
    String dot;
    String s;
    for (s = file; *s && !isSLASH(*s); s++)
	;
    if (*s)
	strcpy(dllPath, file);			/* pathname for module     */
    else /* if in this directory, prefix ./ so search won't use the path   */
	sprintf(dllPath, ".%c%s", SLASH, file);
    dot = strrchr(dllPath,'.');        		/* patch file extension    */
    if (dot == NULL || dot == file) {
	dot = dllPath + strlen(dllPath);
    }
    strcpy(dot,DLL_ENDING);
    return getDLL(dllPath);
}

String mkFFIFilename2(file)    
String file; {
#if HAVE_REALPATH && !HAVE__FULLPATH
    static char path[MAXPATHLEN+1];
#else
    static char path[FILENAME_MAX+1];
#endif
    String dot = strrchr(file,'.');             /* patch file extension    */
    if (isNull(dot))
        dot = file + strlen(file);
    strcpy(path, file);
    strcpy(path + (dot - file),DLL_ENDING);
    return path;
}

String mkFFIFilename(file)                      /* get DLL path for module */
String file; {
#if HAVE_REALPATH && !HAVE__FULLPATH
    static char path[MAXPATHLEN+1];
#else
    static char path[FILENAME_MAX+1];
#endif
    String dot = strrchr(file,'.');             /* patch file extension    */
    if (isNull(dot))
        dot = file + strlen(file);
    strcpy(path, file);
    strcpy(path + (dot - file),".c");
    return path;
}

#if LEADING_UNDERSCORE
#  define INIT_MODULE_FUN  "_initModule"
#  define API_VERSION_FUN  "_HugsAPIVersion"
#else
#  define INIT_MODULE_FUN  "initModule"
#  define API_VERSION_FUN  "HugsAPIVersion"
#endif

Void needPrims(version,dll)   /* Load dll containing prims for current module */
Int  version;
void*  dll; {
    if (havePlugin(textToStr(module(currentModule).text))) {
	return;
    }
    /* Version 2-5: the Haskell module specifies what module to expect
     *              (via a needPrims_hugs decl).
     *
     * Version 0:   the extension DLL specifies the API version it assumes.
     */
    switch (version) { 
    case 2 :
    case 3 :
    case 4 :
	{ 
	    InitModuleFun4 initModule;
	    if (!dll) dll = getModDLL(scriptFile);
	    initModule = (InitModuleFun4)getDLLSymbol(dll,INIT_MODULE_FUN);
	    if (initModule) {
	        Bool flg = setOldDLLFlag(TRUE);
		(*initModule)(hugsAPI4()); 
		setScriptPrims(setPrimInfoDll(dll));
		setOldDLLFlag(flg);
		return;
	    }
	    break;
	}
    case 5 : 
	{ 
	    InitModuleFun5 initModule;
	    if (!dll) dll = getModDLL(scriptFile);
	    initModule = (InitModuleFun5)getDLLSymbol(dll,INIT_MODULE_FUN);
	    if (initModule) {
	        Bool flg = setOldDLLFlag(FALSE);
		(*initModule)(hugsAPI5()); 
		setScriptPrims(setPrimInfoDll(dll));
		setOldDLLFlag(flg);
		return;
	    }
	    break;
	}
    case 0 : 
        {
	    APIVersionFun versionFun;
	    Int   version = 5;

	    dll = getModDLL(scriptFile);
	    versionFun = (APIVersionFun)getDLLSymbol(dll,API_VERSION_FUN);
	    if (versionFun) {
	      version = (*versionFun)();
	    } 
	    needPrims(version, dll);
	    return;
	}
    default: 
	{
	    ERRMSG(0) "This version of Hugs does not support FFI version %d\n", version
	    EEND;
	}
    }
    ERRMSG(0) "Unable to load FFI primitives\n"
    EEND;
}


/* --------------------------------------------------------------------------
 * Compile and link an ffi file which can be dynamically loaded using
 * the above mechanisms.
 * ------------------------------------------------------------------------*/

#define BUFSIZE 1000
static char buffer[BUFSIZE];
static Int used = 0;
static Void local insert     Args((String));
static Void local insertPath Args((String));
static Void local insertChar Args((Char));

static Void local insert(s)
String s; {
    Int l = strlen(s);
    if (used + l + 1 >= BUFSIZE) {
        ERRMSG(0) "Unable to build compilation command"
        EEND;
    }
    strcpy(buffer+used,s);
    used += l;
}

/* Convert backslashes, because they can cause problems with system() */
static Void local insertPath(s)
String s; {
    Int l = strlen(s);
    if (used + l + 1 >= BUFSIZE) {
        ERRMSG(0) "Unable to build compilation command"
        EEND;
    }
    while (*s) {
	buffer[used++] = *s == SLASH ? '/' : *s;
	s++;
    }
}

static Void local insertChar(c)
Char c; {
    char s[2];
    s[0] = c;
    s[1] = '\0';
    insert(s);
}
#undef BUFSIZE

Void compileAndLink(fn,flags)
String fn; 
String flags; {
    char* i = fn;
 
    used    = 0;

#if defined(MKDLL_VISUAL_STUDIO)
    /* find the location of ffihugs.bat */
    /* is in the same directory */
    {
	char Buffer[MAX_PATH+1];
	char Buffer2[MAX_PATH+1];
	GetModuleFileName(GetModuleHandle(NULL), Buffer, MAX_PATH);
	strcpy(strrchr(Buffer, '.'), ".bat");
	GetShortPathName(Buffer, Buffer2, MAX_PATH);

	insert(Buffer2);
	insert(MKDLL_CMD);
    }
#elif defined(MKDLL_CMD)
    /* The compile and link command */
    insert(MKDLL_CMD);
#endif
    
    /* Identify ourselves */
    insert(" -D__HUGS__");

    /* the path to HsFFI.h */
    insert(" \"-I");
    insertPath(hugsdir());
    insertChar('/');
    insert("include\"");

    /* the output file */
    insert(" -o \"");
    insert(mkFFIFilename2(i));
    insert("\"");

    /* the file to compile */
    insert(" \"");
    insert(mkFFIFilename(i));
    insert("\"");

    /* compiler and linker flags specified on Hugs command line */
    if (flags) {
        insert(" ");
        insert(flags);
    }

#if 0
    printf("Executing '%s'\n",buffer);
#endif
    if (shellEsc(buffer,TRUE,TRUE) != 0) {
        ERRMSG(0) "Error while running compilation command '%s'", buffer
        EEND;
    }
    used = 0;
}

/* --------------------------------------------------------------------------
 * Read/write values from/to the registry
 * ------------------------------------------------------------------------*/

#if USE_REGISTRY

static Bool local createKey(hKey, regPath, phRootKey, samDesired)
HKEY    hKey;
String  regPath;
PHKEY   phRootKey; 
REGSAM  samDesired; {
    DWORD  dwDisp;
    return RegCreateKeyEx(hKey, regPath,
			  0, "", REG_OPTION_NON_VOLATILE,
			  samDesired, NULL, phRootKey, &dwDisp) 
	   == ERROR_SUCCESS;
}

static Bool local queryValue(hKey, regPath, var, type, buf, bufSize)
HKEY    hKey;
String  regPath;
String  var;
LPDWORD type;
LPBYTE  buf;
DWORD   bufSize; {
    HKEY hRootKey;

    if (!createKey(hKey, regPath, &hRootKey, KEY_READ)) {
	return FALSE;
    } else {
	LONG res = RegQueryValueEx(hRootKey, var, NULL, type, buf, &bufSize);
	RegCloseKey(hRootKey);
	return (res == ERROR_SUCCESS);
    }
}

/* Specialised version of queryValue(), which doesn't require
 * you to guess the length of a REG_SZ value. Allocates a big
 * enough buffer (using malloc()) to hold the key's value, which
 * is then returned to the callee (along with the resp. to free the
 * buffer.)
 */
static Bool local queryString(hKey, regPath, var, pString)
HKEY    hKey;
String  regPath;
String  var;
String* pString; {
    HKEY  hRootKey;
    LONG  rc;
    DWORD bufSize;
    DWORD valType = REG_SZ;
    Bool  res = FALSE;

    if (!createKey(hKey, regPath, &hRootKey, KEY_READ)) {
	return FALSE;
    } else {
        /* Determine the length of the entry */
	rc = RegQueryValueEx(hRootKey, var, NULL, &valType, NULL, &bufSize);
	
	if (rc == ERROR_SUCCESS && valType == REG_SZ) {
	  /* Got the length, now allocate the buffer and retrieve the string. */
	  if ((*pString = (String)malloc(sizeof(char) * (bufSize + 1))) != NULL) {
	    rc = RegQueryValueEx(hRootKey, var, NULL, &valType, (LPBYTE)*pString, &bufSize);
	    res = (rc == ERROR_SUCCESS);
	  }
	}

	RegCloseKey(hRootKey);
	return (res);
    }
}

static Bool local setValue(hKey, regPath, var, type, buf, bufSize)
HKEY   hKey;
String regPath;
String var;
DWORD  type;
LPBYTE buf;
DWORD  bufSize; {
    HKEY hRootKey;

    if (!createKey(hKey, regPath, &hRootKey, KEY_WRITE)) {
	return FALSE;
    } else {
	LONG res = RegSetValueEx(hRootKey, var, 0, type, buf, bufSize);
	RegCloseKey(hRootKey);
	return (res == ERROR_SUCCESS);
    }
}

String readRegString(HKEY key, String regPath, String var, String def) /* read String from registry */
{
    char* stringVal;
    
    if (queryString(key, regPath, var, &stringVal)) {
        /* The callee is responsible for freeing the returned string */
	return (String)stringVal;
    } else {
        /* Create a *copy* of the default string, so that it can be freed
	   without worry. */
      if ((stringVal = malloc(sizeof(char) * (strlen(def) + 1))) == NULL) {
	return NULL;
      } else {
	strcpy(stringVal, def);
	return (String)stringVal;
      }
    }
}

Bool writeRegString(var,val)      /* write String to registry */
String var;                        
String val; {
    String realVal = ( (NULL == val) ? "" : val);

    return setValue(HKEY_CURRENT_USER, hugsRegRoot, var,
		    REG_SZ, (LPBYTE)realVal, lstrlen(realVal)+1);
}

#if HUGS_FOR_WINDOWS
Bool writeRegInt(var,val)         /* write String to registry */
String var;                        
Int    val; {
    return setValue(HKEY_CURRENT_USER, hugsRegRoot, var, 
		    REG_DWORD, (LPBYTE)&val, sizeof(val));
}

Int readRegInt(var, def)          /* read Int from registry */
String var;
Int    def; {
    DWORD buf;
    DWORD type;

    if (queryValue(HKEY_CURRENT_USER, hugsRegRoot, var, &type, 
		   (LPBYTE)&buf, sizeof(buf))
	&& type == REG_DWORD) {
	return (Int)buf;
    } else if (queryValue(HKEY_LOCAL_MACHINE, hugsRegRoot, var, &type, 
			  (LPBYTE)&buf, sizeof(buf))
	       && type == REG_DWORD) {
	return (Int)buf;
    } else {
	return def;
    }
}
#endif

/* concatenate together all strings from registry of the form regPath\\*\\var,
 * separated by PATHSEP.
 */
String readRegChildStrings(HKEY key, String regPath, String var, String def)
{
  HKEY baseKey;
  ULONG ulResult;
  int done = 0;
  DWORD dwIndex = 0;
  char subKeyName[256];
  DWORD subKeyLen;
  BOOL addedPath = FALSE;

  char* resPath = NULL; /* result path, returned to caller */
  String component;
  FILETIME ft; /* just to satisfy RegEnumKeyEx() */
  char sepString[2];
  StringBuilder* builder = newStringBuilder(0);
  
  sepString[0] = PATHSEP;
  sepString[1] = '\0';
  
  ulResult = RegOpenKeyEx(key, regPath, 0, KEY_READ, &baseKey);
  if (ulResult != ERROR_SUCCESS) {
      freeStringBuilder(builder);
      resPath = strCopy(def);
      return resPath;
  }

  appendString(builder, def);
  while (!done) {
    subKeyLen = sizeof(subKeyName);
    ulResult = RegEnumKeyEx(baseKey, dwIndex, subKeyName, &subKeyLen,
			    NULL, NULL, NULL, &ft);
    if (ulResult == ERROR_SUCCESS) {
      /* read next component of path */
      component = readRegString(baseKey, subKeyName, var, "");
      
      if (addedPath) {
	  appendString(builder,sepString);
      }
      addedPath = TRUE;
      appendString(builder,component);
      free(component); /* readRegString() dynamically allocated it, so let go. */
    } else {
      if (ulResult == ERROR_NO_MORE_ITEMS) {
	done = 1;
      }
    }

    dwIndex++;
  }

  RegCloseKey(baseKey);
  resPath = strCopy(toString(builder));
  freeStringBuilder(builder);
  return resPath;
}
#endif /* USE_REGISTRY */

/* --------------------------------------------------------------------------
 * Platform initialisation 
 * ------------------------------------------------------------------------*/
extern Bool initSystem  Args((Void));
Bool local
initSystem()
{
  /* Called right away by main()  */
#if __MWERKS__ && macintosh
    strcpy(macHugsDir,currentDir());
    SIOUXSettings.autocloseonquit   = true;
    SIOUXSettings.asktosaveonclose  = false;
    SIOUXSettings.columns           = 80;
    SIOUXSettings.rows              = 40; 
    SIOUXSettings.tabspaces         = 8;
    SIOUXSettings.enabledraganddrop = true;
    SIOUXSetTitle("\pHugs 98");
    
#endif
#if HAVE_LOCALE_H
    setlocale(LC_CTYPE, "");
#endif
    return TRUE;
}

/* --------------------------------------------------------------------------
 * Machine dependent control:
 * ------------------------------------------------------------------------*/

Void machdep(what)                      /* Handle machine specific         */
Int what; {                             /* initialisation etc..            */
    switch (what) {
	case MARK    :
#if FLOATS_SUPPORTED
#if BREAK_FLOATS
		       mark(bfTemp);
#endif
		       mark(bdTemp);
#endif
		       break;
	case INSTALL : installHandlers();
		       break;
	case RESET   :
#if FLOATS_SUPPORTED
#if BREAK_FLOATS
		       bfTemp = NIL;
#endif
		       bdTemp = NIL;
#endif
	case BREAK   :
	case EXIT    : normalTerminal();
#if HUGS_FOR_WINDOWS
		       if (what==EXIT)
			   WinHugsExit();
		       else
			   SetCursor(LoadCursor(NULL,IDC_ARROW));
#endif
		       break;
    }
}

/*-------------------------------------------------------------------------*/
