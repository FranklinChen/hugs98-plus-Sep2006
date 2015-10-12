/*
 * Primitives needed to implement the Haskell 98 Directory module.
 *
 * This file has to be included by builtin.c, and won't compile on its own.
 */
 

/* --------------------------------------------------------------------------
 * Directory control:
 * ------------------------------------------------------------------------*/

static Void dirControl Args((Int));

static Void dirControl(what)
Int what; {
}

/* --------------------------------------------------------------------------
 * Directory primitive table:
 * ------------------------------------------------------------------------*/

PROTO_PRIM(primCreateDirectory);
PROTO_PRIM(primRemoveDirectory);
PROTO_PRIM(primRemoveFile);
PROTO_PRIM(primRenameDirectory);
PROTO_PRIM(primRenameFile);
PROTO_PRIM(primGetDirectory);
PROTO_PRIM(primSetDirectory);
PROTO_PRIM(primFileExist);
PROTO_PRIM(primDirExist);
PROTO_PRIM(primGetPermissions);
PROTO_PRIM(primSetPermissions);
PROTO_PRIM(primGetDirContents);
PROTO_PRIM(primGetModTime);

static struct primitive dirPrimTable[] = {
  {"createDirectory",      1+IOArity, primCreateDirectory},
  {"removeDirectory",      1+IOArity, primRemoveDirectory},
  {"removeFile",           1+IOArity, primRemoveFile},
  {"renameDirectory",      2+IOArity, primRenameDirectory},
  {"renameFile",           2+IOArity, primRenameFile},
  {"getCurrentDirectory",  0+IOArity, primGetDirectory},
  {"setCurrentDirectory",  1+IOArity, primSetDirectory},
  {"doesFileExist",        1+IOArity, primFileExist},
  {"doesDirectoryExist",   1+IOArity, primDirExist},
  {"getPerms",             1+IOArity, primGetPermissions},
  {"setPerms",             5+IOArity, primSetPermissions},
  {"getDirContents",       1+IOArity, primGetDirContents},
  {"getModTime",           1+IOArity, primGetModTime},

  {0,			0, 0}
};

static struct primInfo dirPrims = { dirControl, dirPrimTable, 0 };

static	Bool	local	isDirectory	Args((String));

#define ToBool(v) ( (v) ? nameTrue : nameFalse)

#ifdef _MSC_VER
/* If not provided, define em. */
#ifndef R_OK
#define R_OK 04
#endif
#ifndef W_OK
#define W_OK 02
#endif
#ifndef X_OK
#define X_OK 06
#endif
#endif

/* MSVC6 doesn't define these helper macros in <sys/stat.h> there
 * might be other platforms too, so... The assumption here is that
 * S_ISDIR() and friends indeed are CPP macros - if that's not the
 * case, please adjust the conditional below to suit your platform
 * (and feed back the tweak you make.)
 */
#if !defined(S_ISDIR)
#define S_ISDIR(st_mode)  ((st_mode & S_IFMT) == S_IFDIR)
#endif

#if !defined(S_ISREG)
#define S_ISREG(st_mode)  ((st_mode & S_IFMT) == S_IFREG)
#endif

/* --------------------------------------------------------------------------
 * Directory primitives:
 * ------------------------------------------------------------------------*/

primFun(primCreateDirectory) { /* create a directory, :: String -> IO ()   */
  int rc;
  String s = evalName(IOArg(1));
  
  if (!s) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.createDirectory",
		     "illegal directory name",
		     &IOArg(1)));
  }
  
#if defined(_MSC_VER) || mingw32_HOST_OS
   rc = mkdir(s);
#else
   rc = mkdir(s,0777);
#endif
   if (rc != 0)
      throwErrno("Directory.createDirectory", FALSE, NO_HANDLE, &IOArg(1));
  IOReturn(nameUnit);
}

primFun(primRemoveDirectory) { /* remove a directory	   */
  int rc;
  String s = evalName(IOArg(1));
  
  if (!s) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.removeDirectory",
		     "illegal directory name",
		     &IOArg(1)));
  }
  
   rc = rmdir(s);

   if (rc != 0)
      throwErrno("Directory.removeDirectory", FALSE, NO_HANDLE, &IOArg(1));
  IOReturn(nameUnit);
}

primFun(primRemoveFile) { /* remove a file	   */
  int rc;
  String s = evalName(IOArg(1));
  
  if (!s) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.removeFile",
		     "illegal file name",
		     &IOArg(1)));
  }
  
   rc = unlink(s);

  if (rc != 0)
    throwErrno("Directory.removeFile", TRUE, NO_HANDLE, &IOArg(1));
  IOReturn(nameUnit);
}

/* Pair of macros for creating temporary strings */
#if HAVE_ALLOCA
# define ALLOC_STRING(x) (String)alloca(sizeof(char)*(x + 1))
# define FREE_STRING(x)
#elif HAVE__ALLOCA
# define ALLOC_STRING(x) (String)_alloca(sizeof(char)*(x + 1))
# define FREE_STRING(x)
#else
# define ALLOC_STRING(x) (String)malloc(sizeof(char)*(x + 1))
# define FREE_STRING(x)  free(x)
#endif

primFun(primRenameDirectory) { /* rename a directory	   */
  int rc;
  String tmpStr;
  String to;
  String from;

  tmpStr = evalName(IOArg(1));
  if (!tmpStr) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.renameDirectory",
		     "illegal directory name",
		     &IOArg(1)));
  }
  to = ALLOC_STRING(strlen(tmpStr));
  strcpy(to, tmpStr);

  from = evalName(IOArg(2));
  if (!from) {
    FREE_STRING(to);
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.renameDirectory",
		     "illegal directory name",
		     &IOArg(2)));
  }

  rc = rename(from,to);

  FREE_STRING(to);

  if (rc != 0)
    throwErrno("Directory.renameDirectory", FALSE, NO_HANDLE, &IOArg(1));
  IOReturn(nameUnit);
}

primFun(primRenameFile) { /* rename a file	   */
  int rc;
  String tmpStr;
  String to;
  String from;

  tmpStr = evalName(IOArg(1));
  if (!tmpStr) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.renameFile",
		     "illegal file name",
		     &IOArg(1)));
  }
  to = ALLOC_STRING(strlen(tmpStr));
  strcpy(to, tmpStr);

  from = evalName(IOArg(2));
  if (!from) {
    FREE_STRING(to);
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.renameFile",
		     "illegal file name",
		     &IOArg(2)));
  }

  if (isDirectory(from)) {
    FREE_STRING(to);
    IOFail(mkIOError(NULL,
		     namePermDenied,
		     "Directory.renameFile",
		     "is a directory",
		     &IOArg(2)));
  }

  if (isDirectory(to)) {
    FREE_STRING(to);
    IOFail(mkIOError(NULL,
		     namePermDenied,
		     "Directory.renameFile",
		     "is a directory",
		     &IOArg(1)));
  }

  rc = rename(from,to);
  
  FREE_STRING(to);

  if (rc != 0)
    throwErrno("Directory.renameFile", TRUE, NO_HANDLE, &IOArg(1));
  IOReturn(nameUnit);
}


primFun(primGetDirectory) { /* IO String - get current directory. */
  char buffer[FILENAME_MAX+1];
  if ((char*)(getcwd(buffer,FILENAME_MAX)) == (char*)NULL)
    throwErrno("Directory.getCurrentDirectory", FALSE, NO_HANDLE, NULL);
  pushString(buffer);
  IOReturn(pop());
}

primFun(primSetDirectory) { /* String -> IO () - set current directory. */
  int rc;
  String s = evalName(IOArg(1));

  if (!s) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.setCurrentDirectory",
		     "illegal directory name",
		     &IOArg(1)));
  }
  
   rc = chdir(s);

   if (rc != 0)
      throwErrno("Directory.setCurrentDirectory", FALSE, NO_HANDLE, &IOArg(1));
   IOReturn(nameUnit);
}

primFun(primFileExist) { /* FilePath -> IO Bool - check to see if file exists. */
  int rc;
  String s = evalName(IOArg(1));
  struct stat st;

  if (!s) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.doesFileExist",
		     "illegal file name",
		     &IOArg(1)));
  }
  
  rc = stat(s, &st);
  
  IOBoolResult(rc == 0 && !S_ISDIR(st.st_mode) );
}

primFun(primDirExist) { /* FilePath -> IO Bool - check to see if directory exists. */
  String s = evalName(IOArg(1));

  if (!s) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.doesDirectoryExist",
		     "illegal directory name",
		     &IOArg(1)));
  }
  IOBoolResult(isDirectory(s));
}

static Bool local isDirectory(s)
String s; {
  int rc;
  struct stat st;
#ifdef _WIN32
  /* For whatever reason, stat()ing a directory name
   * like "foo/" returns an error, while both "foo" and "foo/."
   * is fine. We want them all to be treated equal.
   */
  int len = strlen(s);
  while (len > 0 && 
	 (s[len-1] == '/' || s[len-1] == '\\')) {
      s[len-1] = '\0';
      len--;
  }
#endif
  rc = stat(s, &st);
  return (rc==0 && S_ISDIR(st.st_mode));
}

primFun(primGetPermissions) { /* FilePath -> IO (Bool,Bool,Bool,Bool) */
  int rc;
  String s = evalName(IOArg(1));
  struct stat st;
  int isR, isW, isX;
  

#if __MWERKS__ && macintosh
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.primGetPermissions",
		     "operation not supported",
		     &IOArg(1)));
#else
  if (!s) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.getPermissions",
		     "illegal file name",
		     &IOArg(1)));
  }
  
  isR = access(s, R_OK);
  isW = access(s, W_OK);
  isX = access(s, X_OK);
  rc = stat(s, &st);
  
  if (rc != 0)
    throwErrno("Directory.getPermissions", FALSE, NO_HANDLE, &IOArg(1));
  IOReturn(ap(ap(ap(ap( mkTuple(4),
			ToBool(isR == 0)),
		    ToBool(isW == 0)),
		 ToBool(isX == 0 && !S_ISDIR(st.st_mode))),
	      ToBool(isX == 0 && S_ISDIR(st.st_mode))));
#endif
}

#define EVAL_BOOL(x,y) \
   eval(y);\
   if (whnfHead==nameTrue) { \
      x = TRUE; \
   } else if (whnfHead==nameFalse) { \
      x = FALSE; \
   } else { \
      IOFail(mkIOError(NULL, \
	     nameIllegal, \
	     "Directory.setPermissions", \
	     "illegal flag", \
	     NULL)); \
   }

#ifdef _MSC_VER
#define READ_FLAG   S_IREAD
#define WRITE_FLAG  S_IWRITE
#define EXEC_FLAG   S_IEXEC
#else
#define READ_FLAG   S_IRUSR
#define WRITE_FLAG  S_IWUSR
#define EXEC_FLAG   S_IXUSR
#endif

#define SET_CHMOD_FLAG(x,y)  (x ? y : 0)

primFun(primSetPermissions) { /* FilePath -> Bool -> Bool -> Bool -> Bool -> IO () */
  int rc;
  String str;
  struct stat st;

  Bool   r;
  Bool   w;
  Bool   e;
  Bool   s;
  
#if __MWERKS__ && macintosh
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.primSetPermissions",
		     "operation not supported",
		     &IOArg(1)));
#else
  EVAL_BOOL(s, IOArg(1));
  EVAL_BOOL(e, IOArg(2));
  EVAL_BOOL(w, IOArg(3));
  EVAL_BOOL(r, IOArg(4));
  
  str = evalName(IOArg(5));
  
  if (!str) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.setPermissions",
		     "illegal file name",
		     &IOArg(5)));
  }

  rc = stat(str, &st);
  if (rc == 0)
    rc = chmod(str,
	       (st.st_mode & ~(READ_FLAG|WRITE_FLAG|EXEC_FLAG)) |
	       SET_CHMOD_FLAG(r, READ_FLAG)  |
	       SET_CHMOD_FLAG(w, WRITE_FLAG) |
	       SET_CHMOD_FLAG(e||s, EXEC_FLAG));
	     
  if (rc != 0)
    throwErrno("Directory.setPermissions", TRUE, NO_HANDLE, &IOArg(5));
  IOReturn(nameUnit);
  
#endif
}

/* Pedantically remove these local defs. */
#undef READ_FLAG
#undef WRITE_FLAG
#undef EXEC_FLAG
#undef SET_CHMOD_FLAG

primFun(primGetDirContents) { /* FilePath -> IO [FilePath] */
#ifdef _MSC_VER
  /* The MS CRT doesn't provide opendir()/readdir(), but uses
     the 'std' MS find first/next/close group of functions for
     iterating over the contents of a directory. */
  int rc;
  long dirHandle;
  struct _finddata_t fData;
  char buffer[FILENAME_MAX+20];
  struct stat st;
  Cell ls;
  String fName = evalName(IOArg(1));
  
  if (!fName) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.getDirectoryContents",
		     "illegal directory name",
		     &IOArg(1)));
  }
  
  if (strlen(fName) > FILENAME_MAX) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.getDirectoryContents",
		     "file name too long",
		     &IOArg(1)));
  }
  
  /* First, check whether the directory exists... */
  if (stat(fName, &st) < 0)
    throwErrno("Directory.getDirectoryContents", FALSE, NO_HANDLE, &IOArg(1));
  if (!S_ISDIR(st.st_mode)) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.getDirectoryContents",
		     "not a directory",
		     &IOArg(1)));
  }
  
  if (snprintf(buffer,sizeof(buffer)-1,"%s\\*.*",fName) < 0) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.getDirectoryContents",
		     "illegal directory name",
		     &IOArg(1)));
  } else {
      buffer[sizeof(buffer)-1] = '\0';
  }

  dirHandle = _findfirst(buffer, &fData);
  rc = dirHandle;
  
  ls = nameNil;
  
  while (rc >= 0) {
    Cell n;
    push(ls);
    pushString(fData.name);
    n = ap(nameCons, pop());
    ls = ap(n, pop());
    rc = _findnext(dirHandle, &fData);
  }
  if (errno != ENOENT)
    throwErrno("Directory.getDirectoryContents", FALSE, NO_HANDLE, &IOArg(1));

  /* Close and release resources */
  rc = _findclose(dirHandle);
  if (rc == -1 && errno != ENOENT)
    throwErrno("Directory.getDirectoryContents", FALSE, NO_HANDLE, &IOArg(1));
  IOReturn(ls);
#elif HAVE_DIRENT_H
  /* opendir() / readdir() implementation. */
  DIR* dir;
  struct dirent* pDir;
  Cell ls;
  String fName = evalName(IOArg(1));
  
  if (!fName) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.getDirectoryContents",
		     "illegal file name",
		     &IOArg(1)));
  }
  
  dir = opendir(fName);
  
  if (dir == NULL)
    throwErrno("Directory.getDirectoryContents", FALSE, NO_HANDLE, &IOArg(1));

  ls = nameNil;
  
  /* To ensure that the test below doesn't
     succeed just because the impl of readdir()
     'forgot' to reset 'errno', do it ourselves. */
  errno = 0;
  
  while ( (pDir = readdir(dir)) ) {
    Cell n;
    push(ls);
    pushString(pDir->d_name);
    n = ap(nameCons, pop());
    ls = ap(n, pop());
  }
  
  if (errno != 0
#if mingw32_HOST_OS
      && errno != ENOENT
#endif
      ) {
    int rc = errno;
    closedir(dir);
    errno = rc;
    throwErrno("Directory.getDirectoryContents", FALSE, NO_HANDLE, &IOArg(1));
  }

  closedir(dir);
  IOReturn(ls);

#else
  /* Sorry, don't know how to access a directory on your platform */
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.getDirectoryContents",
		     "operation not supported",
		     &IOArg(1)));
#endif
}

primFun(primGetModTime) { /* FilePath -> IO Int{-time_t-} - get the mod. time of the file/directory. */
  int rc;
  String s = evalName(IOArg(1));
  struct stat st;

  if (!s) {
    IOFail(mkIOError(NULL,
		     nameIllegal,
		     "Directory.getModificationTime",
		     "illegal file name",
		     &IOArg(1)));
  }
  
  rc = stat(s, &st);
  
  if (rc < 0)
    throwErrno("Directory.getModificationTime", TRUE, NO_HANDLE, &IOArg(1));
  IOReturn(mkInt(st.st_mtime));
}
