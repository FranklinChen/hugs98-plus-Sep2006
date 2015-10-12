dnl ################################################################
dnl Macros
dnl (hard-core autoconf hackers only)
dnl ################################################################

dnl Like AC_SUBST but with a default value in case var is undefined
dnl typically usage from cshell:  env DEV_NULL="/dev/null" ./configure

dnl AC_SUBST_DEF(varname,defaultvalue)

AC_DEFUN([AC_SUBST_DEF],[
$1=${$1=$2}
AC_SUBST($1)
])


dnl On some machines, you cannot take the address of a jmp_buf
dnl
AC_DEFUN([AC_C_JMPBUF_ARRAY],
[AC_CACHE_CHECK(for arrays of jmp_bufs, ac_cv_c_jmp_buf_array,
[AC_TRY_COMPILE([
#include <setjmp.h>
int test1() {
    jmp_buf jb[1];
    jmp_buf *jbp = jb;
    return (setjmp(jb[0]) == 0);
}
],
[int i;], 
ac_cv_c_jmp_buf_array=yes,
ac_cv_c_jmp_buf_array=no)])
if test "$ac_cv_c_jmp_buf_array" = yes; then
AC_DEFINE(JMPBUF_ARRAY, [1],
  [Define to 1 if jmpbufs can be treated like arrays.])
fi
])


dnl POSIX systems prefer "diff -C 1"; SunOS4 prefers "diff -c1".
dnl
AC_DEFUN([AC_PROG_DIFF],
[AC_PATH_PROG(DIFF,diff)
AC_CACHE_CHECK(whether to use "diff -c1" or "diff -C 1", CONTEXT_DIFF,
if AC_TRY_COMMAND(diff -C 1 config.log config.log); then
  CONTEXT_DIFF="$DIFF -C 1"
else
  if AC_TRY_COMMAND(diff -c1 config.log config.log); then
    CONTEXT_DIFF="$DIFF -c1"
  else
    CONTEXT_DIFF="$DIFF"
  fi
fi
)
AC_SUBST(CONTEXT_DIFF)
])

dnl check for gcc's "labels as values" feature
AC_DEFUN([AC_C_LABELS_AS_VALUES],
[AC_CACHE_CHECK([labels as values], ac_cv_labels_as_values,
[AC_TRY_COMPILE([
int foo(int);
int foo(i)
int i; { 
static void *label[] = { &&l1, &&l2 };
goto *label[i];
l1: return 1;
l2: return 2;
}
],
[int i;], 
ac_cv_labels_as_values=yes,
ac_cv_labels_as_values=no)])
if test "$ac_cv_labels_as_values" = yes; then
AC_DEFINE(HAVE_LABELS_AS_VALUES, [1],
  [Define to 1 if compiler supports gcc's "labels as values"
   (aka computed goto) feature (which is used to speed up instruction
   dispatch in the interpreter).])
fi
])

# FP_DECL_TIMEZONE
# ---------------
# Defines HAVE_DECL_TIMEZONE to 1 if declared, 0 otherwise.
# Defines HAVE_DECL__TIMEZONE to 1 if declared, 0 otherwise.
# Defines HAVE_DECL_ALTZONE to 1 if declared, 0 otherwise.
#
AC_DEFUN([FP_DECL_TIMEZONE],
[AC_REQUIRE([AC_HEADER_TIME])dnl
AC_CHECK_HEADERS([sys/time.h])
AC_CHECK_DECLS([timezone, _timezone, altzone], [], [],
[#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif])
])# FP_DECL_TIMEZONE


# FP_CHECK_FUNC(FUNCTION, PROLOGUE, BODY, [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
# ---------------------------------------------------------------------------------
# A variant of AC_CHECK_FUNCS, limited to a single FUNCTION, but with the
# additional flexibility of specifying the PROLOGUE and BODY.
AC_DEFUN([FP_CHECK_FUNC],
[AS_VAR_PUSHDEF([fp_func], [fp_cv_func_$1])dnl
AC_CACHE_CHECK([for $1], fp_func,
[AC_LINK_IFELSE([AC_LANG_PROGRAM([$2], [$3])],
                [AS_VAR_SET(fp_func, yes)],
                [AS_VAR_SET(fp_func, no)])])
AS_IF([test AS_VAR_GET(fp_func) = yes],
      [AC_DEFINE(AS_TR_CPP(HAVE_$1), [1],
                [Define to 1 if you have the `]$1[' function.]) $4],
      [$5])dnl
AS_VAR_POPDEF([fp_func])dnl
])# FP_CHECK_FUNC


dnl ** Try building and loading a dynamically loadable library using
dnl    the specified flags.
dnl
AC_DEFUN([HUGS_TRY_DYNLINK],
dnl AC_BEFORE([$0], [AC_C_PROTOTYPES])
[AC_MSG_CHECKING(if '$1' builds loadable libraries)
AC_CACHE_VAL(ac_cv_dll_flags,
[
  cat > conftest_dl.c <<EOF
int x = 0;    /* global */
int y;        /* common */
static int z; /* static */
static int test2() { return (test() + x + y + z); }
int test() { return test2(); }
EOF

  ac_mkdll='${CC-cc} $1 conftest_dl.c -o conftest_dl.so 1>&AC_FD_CC'

  if AC_TRY_EVAL(ac_mkdll) && test -s conftest_dl.so 
  then dnl compiling and linking loadee succeeded

    cat > conftest.c << EOF
#include "confdefs.h"
#if PROTOTYPES       /* To enable use of prototypes whenever possible */
#define Args(x) x
#else
#define Args(x) ()
#endif

#define SYMBOL1 "test"
#define SYMBOL2 "_test"

#define CANTRUN  1
#define CANTOPEN 2
#define SYM1_OK  3
#define SYM2_OK  4
#define CANTFIND 5

#if HAVE_DLFCN_H /* eg LINUX, SOLARIS, ULTRIX */

#include <stdio.h>
#include <dlfcn.h>

main()
{
    void *instance;
    void *sym;

    instance = dlopen("./conftest_dl.so",1);
    if (instance==0) exit(CANTOPEN);
      
    sym = dlsym(instance,SYMBOL1);
    if (sym != 0) exit(SYM1_OK);

    sym = dlsym(instance,SYMBOL2);
    if (sym != 0) exit(SYM2_OK);

    exit(CANTFIND);
}

#elif HAVE_DL_H /* eg HPUX */

#include <dl.h>

main()
{
    shl_t instance;
    void* r;

    instance = shl_load("./conftest_dl.so",BIND_IMMEDIATE,0L);
    if (instance == 0) exit(CANTOPEN);
    
    if (0 == shl_findsym(&instance,SYMBOL1,TYPE_PROCEDURE,&r)) exit(SYM1_OK);

    if (0 == shl_findsym(&instance,SYMBOL2,TYPE_PROCEDURE,&r)) exit(SYM2_OK);

    exit(CANTFIND);
}

#elif HAVE_MACH_O_DYLD_H         /* MacOS X */

#include <stdio.h>
#include <mach-o/dyld.h>

main()
{
    NSObjectFileImage ofile;
    NSModule handle = NULL;
    void* addr;
    NSSymbol sym;

    if (NSCreateObjectFileImageFromFile("./conftest_dl.so",&ofile) != NSObjectFileImageSuccess)
        exit(CANTOPEN);

    handle = NSLinkModule(ofile,"./conftest_dl.so",NSLINKMODULE_OPTION_PRIVATE);
    if (handle == 0) exit(CANTOPEN);
    
    sym = NSLookupSymbolInModule(handle, SYMBOL1); 
    if (sym != 0) exit(SYM1_OK);
    
    sym = NSLookupSymbolInModule(handle, SYMBOL2); 
    if (sym != 0) exit(SYM2_OK);
    
    exit(CANTFIND);
}

#elif HAVE_WINDOWS_H

#include <windows.h>

main()
{
    HINSTANCE instance;
    void* sym;

    instance = LoadLibrary("conftest_dl.so");
    if (instance ==0) exit(CANTOPEN);

    sym = (void*)GetProcAddress(instance,SYMBOL1);
    if (sym != 0) exit(SYM1_OK);

    sym = (void*)GetProcAddress(instance,SYMBOL2);
    if (sym != 0) exit(SYM2_OK);

    exit(CANTFIND);
}

#else

main()
{
  exit(CANTRUN);
}

#endif
EOF


if AC_TRY_EVAL(ac_link) && test -s conftest${ac_exeext} 
then dnl compiling and linking loader succeeded

  ./conftest 2>/dev/null
  ac_result=$?
  if test $ac_result = 3; then
    ac_cv_dll_flags='$1'
    ac_cv_leading_underscore=no
  fi
  if test $ac_result = 4; then
    ac_cv_dll_flags='$1'
    ac_cv_leading_underscore=yes
  fi

fi dnl compiling and linking loader succeeded
fi dnl compiling and linking loadee succeeded

rm -fr conftest* a.out
]) dnl close AC_CACHE_VAL
AC_MSG_RESULT($ac_cv_dll_flags)]
)


dnl External macros

builtin([include],ac_macros/acx_pthread.m4)
