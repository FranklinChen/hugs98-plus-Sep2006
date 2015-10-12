dnl--------------------------------------------------------------------
dnl
dnl Hugs98 interpreter configure script template.
dnl Process this file with autoreconf to produce a configure script.
dnl
dnl--------------------------------------------------------------------

AC_INIT([Hugs98], [1.0], [hugs-bugs@haskell.org], [hugs98])
AC_CONFIG_SRCDIR([src/hugs.c])

dnl We need 2.50 for AC_HELP_STRING.
AC_PREREQ([2.50])

AC_CONFIG_SUBDIRS([libraries])

AC_CONFIG_HEADER(src/config.h src/options.h)
AH_TOP([/* platform-specific defines */
#include "platform.h"])

dnl--------------------------------------------------------------------
dnl Choose host(/target/build) platform
dnl--------------------------------------------------------------------

dnl Guess host/target/build platform(s) if necessary.
AC_CANONICAL_TARGET

dnl ** GHC does further canonicalization
BuildPlatform=`./fp-platform $build`
HostPlatform=`./fp-platform $host`
TargetPlatform=`./fp-platform $target`

AC_SUBST([HostPlatform])

if test x"$HostPlatform" != x"$BuildPlatform" ; then
  AC_MSG_ERROR([Hugs98 does not yet support differing build/host (i.e., cross-compiling)])
fi

fp_get_cpu='s/-.*//'
fp_get_os='s/^[[^-]]*-[[^-]]*-\([[^-]]*\).*/\1/'

build_cpu=`echo $BuildPlatform | sed "$fp_get_cpu"`
build_os=`echo $BuildPlatform | sed "$fp_get_os"`
host_cpu=`echo $HostPlatform | sed "$fp_get_cpu"`
host_os=`echo $HostPlatform | sed "$fp_get_os"`
target_cpu=`echo $TargetPlatform | sed "$fp_get_cpu"`
target_os=`echo $TargetPlatform | sed "$fp_get_os"`

# We don't use AS_TR_CPP here, because it changes case too.
fp_tr_cpp="sed s/[[^_a-zA-Z0-9]]/_/g"

AC_SUBST([BuildPlatform_CPP],  [`echo $BuildPlatform  | $fp_tr_cpp`])
AC_SUBST([HostPlatform_CPP],   [`echo $HostPlatform   | $fp_tr_cpp`])
AC_SUBST([TargetPlatform_CPP], [`echo $TargetPlatform | $fp_tr_cpp`])

AC_SUBST([BuildArch_CPP],  [`echo $build_cpu  | $fp_tr_cpp`])
AC_SUBST([HostArch_CPP],   [`echo $host_cpu   | $fp_tr_cpp`])
AC_SUBST([TargetArch_CPP], [`echo $target_cpu | $fp_tr_cpp`])

AC_SUBST([BuildOS_CPP],  [`echo $build_os  | $fp_tr_cpp`])
AC_SUBST([HostOS_CPP],   [`echo $host_os   | $fp_tr_cpp`])
AC_SUBST([TargetOS_CPP], [`echo $target_os | $fp_tr_cpp`])

dnl--------------------------------------------------------------------
dnl
dnl Check Configuration options
dnl
dnl--------------------------------------------------------------------

AC_ARG_ENABLE(path-canonicalization,
  [AC_HELP_STRING([--enable-path-canonicalization],
     [enable canonicalization of filenames])],
  [if test "$enableval" = yes; then
   AC_DEFINE([PATH_CANONICALIZATION], [1],
     [Define to 1 if you want filenames to be converted to normal form
      by: (a) replacing relative pathnames with absolute pathnames and
      eliminating .. and . where possible. (b) converting to lower case
      (only in case-insensitive filesystems)])
   fi])

AC_ARG_ENABLE(timer,
  [AC_HELP_STRING([--enable-timer],
     [enable evaluation timing (for benchmarking Hugs)])])

AC_ARG_ENABLE(profiling,
  [AC_HELP_STRING([--enable-profiling],
     [enable heap profiler])],
  [if test "$enableval" = yes; then
   AC_DEFINE([PROFILING], [1],
     [Define to 1 if heap profiling should be used.])
   fi])

AC_ARG_ENABLE(stack-dumps,
  [AC_HELP_STRING([--enable-stack-dumps],
     [enable stack dump on stack overflow])],
  [if test "$enableval" = yes; then
   AC_DEFINE([GIMME_STACK_DUMPS], [1],
     [If you get really desperate to understand why your Hugs programs
      keep crashing or running out of stack, you might like to set this
      flag and recompile Hugs. When you hit a stack error, it will print
      out a list of all the objects currently under evaluation. The
      information isn't perfect and can be pretty hard to understand but
      it's better than a poke in the eye with a blunt stick. This is a
      very experimental feature!])
   fi])

AC_ARG_WITH(nmake,
  [AC_HELP_STRING([--with-nmake],
     [produce a Makefile compatible with nmake])],
  [if test "$withval" = yes; then RM="del";CP="copy"; fi])

AC_ARG_ENABLE(large-banner,
  [AC_HELP_STRING([--disable-large-banner],
     [disable multiline startup banner])],
  [if test "$enableval" = no; then
   AC_DEFINE([SMALL_BANNER], [1],
     [Define to 1 if you want the small startup banner.])
   fi])

AC_ARG_WITH(gui,
  [AC_HELP_STRING([--with-gui],
     [build Hugs for Windows GUI (Borland C++ only)])])

AC_ARG_ENABLE(internal-prims,
  [AC_HELP_STRING([--enable-internal-prims],
     [experimental primitives to access Hugs's innards])],
  [if test "$enableval" = yes; then
   AC_DEFINE([INTERNAL_PRIMS], [1],
     [Define to 1 if you want to use the primitives which let you examine
      Hugs internals.])
   AC_DEFINE([BYTECODE_PRIMS], [1],
     [Define to 1 if you want to use the primitives which let you
      examine Hugs bytecodes (requires INTERNAL_PRIMS).])
   fi])

AC_ARG_ENABLE(debug,
  [AC_HELP_STRING([--enable-debug],
     [include C debugging information (for debugging Hugs)])],
  [if test "$enableval" = yes; then
   AC_DEFINE([DEBUG_CODE], [1],
     [Define to 1 if debugging generated bytecodes or the bytecode
      interpreter.])
   AC_DEFINE([DEBUG_PRINTER], [1],
     [Define if you want to use a low-level printer from within a
      debugger.])
   fi])

AC_ARG_ENABLE(tag-checks,
  [AC_HELP_STRING([--enable-tag-checks],
     [runtime tag checking (for debugging Hugs)])],
  [if test "$enableval" = yes; then
   AC_DEFINE([CHECK_TAGS], [1],
     [Define to 1 if you want to perform runtime tag-checks as an
      internal consistency check. This makes Hugs run very slowly - but is
      very effective at detecting and locating subtle bugs.])
   fi])

AC_ARG_ENABLE(lint,
  [AC_HELP_STRING([--enable-lint],
     [enable "lint" flags (for debugging Hugs)])])

AC_ARG_ENABLE(only98,
  [AC_HELP_STRING([--enable-only98],
     [build Hugs to understand Haskell 98 only])],
  [if test "$enableval" = yes; then
   AC_DEFINE([HASKELL_98_ONLY], [1],
     [Define to 1 to omit Hugs extensions])
   fi])

AC_ARG_WITH(pthreads,
  [AC_HELP_STRING([--with-pthreads],
     [build Hugs using POSIX threads C library])],
  [if test "$withval" = yes; then
   # needed with pthreads
   AC_DEFINE([DONT_PANIC], [1],
     [In a plain Hugs system, most signals (SIGBUS, SIGTERM, etc) indicate
      some kind of error in Hugs - or maybe a stack overflow.  Rather than
      just crash, Hugs catches these errors and returns to the main loop.
      It does this by calling a function "panic" which longjmp's back to
      the main loop.  If you're developing a foreign library, this may not
      be the right behaviour - it's better if Hugs leaves them for your
      debugger to catch rather than trapping them and "panicking".])
   fi])

AC_ARG_ENABLE(ffi,
  [AC_HELP_STRING([--enable-ffi],
     [include modules that use the FFI [default=autodetect]])],
  , [enable_ffi=autodetect])

AC_ARG_ENABLE(char-encoding,
  [AC_HELP_STRING([--enable-char-encoding],
     [encode all character I/O using the byte encoding determined by the
      locale in effect at that time.  To require that the UTF-8 encoding
      is always used, give the --enable-char-encoding=utf8 option.
      [default=autodetect]])],
  , [enable_char_encoding=autodetect])

dnl--------------------------------------------------------------------
dnl
dnl Check for programs
dnl
dnl--------------------------------------------------------------------

dnl ToDo: if we have YACC, insert parser.c: parser.y in Makefile
AC_PROG_YACC
dnl To implement the Haskell layout rule of inserting an implicit close
dnl brace if it would be legal where an illegal token is encountered,
dnl parser.y does some error recovery trickery that is incompatible
dnl with byacc.
if test x"`basename ${YACC}`" = x"byacc"; then
  AC_MSG_ERROR([Found 'byacc', but the Hugs98 parser is incompatible with it.
You need to install 'bison' and re-run the configure script.])
fi

: ${CFLAGS=-g}
OPTFLAGS="-O2"

AC_PROG_CC
AC_PROG_GCC_TRADITIONAL

if test "$with_pthreads" = yes; then
   ACX_PTHREAD
   CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
   LIBS="$PTHREAD_LIBS $LIBS"
   CC="$PTHREAD_CC"
fi

dnl  The following patch avoids the outburst of (benign) error messages during 
dnl  compilation of machine.c under MacOS X/Darwin, as well as some linker 
dnl  warnings caused by the default two-level namespace introduced in version 10.1
dnl
case $HostPlatform in
  *-*-darwin*)
     CFLAGS="$CFLAGS -no-cpp-precomp"
     LDFLAGS="$LDFLAGS -flat_namespace"
     ;;
  # As suggested by James B. White III (Trey) <whitejbiii@ornl.gov> (for
  # AIX 4.3.3 & XL C 5.0.2 at least, may need to tweak the pattern match
  # below some to avoid upsetting other AIX versions).
  *-*-aix*) 
     optcflags="-qalloca"
     CFLAGS_save="$CFLAGS"
     CFLAGS="$CFLAGS $optcflags"
     AC_MSG_CHECKING([whether $CC accepts $optcflags])
     AC_LANG_C
     AC_TRY_LINK([],[int main(){return(0);}],[optok=yes],[optok=no])
     if test "$optok" = "yes"; then
	# lump the recognition of -qalloca together with -qmaxmem=-1 and -bmaxdata..
        CFLAGS="$CFLAGS_save $optcflags"
	LDFLAGS="$LDFLAGS -bmaxdata:0x70000000"; 
	OPTFLAGS="-O -qmaxmem=-1"
        AC_MSG_RESULT([yes])
     else
        CFLAGS="$CFLAGS_save"
	AC_MSG_RESULT([no])
     fi
     ;;
esac

if test "x$CP" != "xcopy"; then
# Only check for RM&CP if they haven't been overridden already (cf. --with-nmake)
AC_PATH_PROG(RM,rm)
RM="$RM -f"
AC_PATH_PROG(CP,cp)
fi

AC_CHECK_PROG(LD,ld,ld)

AC_CHECK_PROGS(PERL,perl)  dnl should test for perl5
AC_PROG_MAKE_SET
AC_CHECK_PROG(have_hp2ps,hp2ps,1,0)
if test "$have_hp2ps" = "1"; then
AC_DEFINE(HAVE_HP2PS,1,
  [Define to 1 if heap profiler can (and should) automatically invoke
   hp2ps to convert heap profile (in "profile.hp") to PostScript.])
fi
AC_CHECK_PROGS(DOCBOOK2HTML,[docbook2html db2html],[])
AC_CHECK_PROGS(DOCBOOK2DVI,[docbook2dvi db2dvi],[])
AC_CHECK_PROGS(DOCBOOK2PDF,[docbook2pdf db2pdf],[])
AC_CHECK_PROGS(DOCBOOK2PS,[docbook2ps db2ps],[])

dnl AC_PROG_INSTALL

dnl POSIX systems prefer "diff -C 1"; SunOS4 prefers "diff -c1".
AC_PROG_DIFF

dnl If you can run configure, you certainly have /bin/sh
AC_DEFINE(HAVE_BIN_SH, 1, [Define to 1 if you have /bin/sh])

dnl--------------------------------------------------------------------
dnl 
dnl Check for libraries
dnl
dnl--------------------------------------------------------------------

AC_CHECK_LIB([dl], [dlopen])
AC_CHECK_LIB([dld], [shl_load])
AC_CHECK_LIB([m], [atan])

dnl--------------------------------------------------------------------
dnl
dnl Check for header files
dnl
dnl--------------------------------------------------------------------

AC_HEADER_STDC
dnl checks for sys/wait.h
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS(stdarg.h stdlib.h unistd.h assert.h ctype.h string.h limits.h)
AC_CHECK_HEADERS(fcntl.h sgtty.h termio.h termios.h signal.h)
AC_CHECK_HEADERS(sys/types.h sys/stat.h sys/ioctl.h sys/resource.h sys/param.h)
AC_CHECK_HEADERS(console.h Files.h errno.h stat.h direct.h dirent.h)

AC_HEADER_TIME
AC_CHECK_HEADERS(time.h sys/time.h sys/timeb.h sys/times.h)

dnl One of these two is used to obtain floating point parameters
AC_CHECK_HEADERS(float.h values.h)

dnl DOS include files
AC_CHECK_HEADERS(dos.h conio.h io.h std.h) 

dnl Windows include files
AC_CHECK_HEADERS(windows.h)

dnl dynamic loading include files
AC_CHECK_HEADERS([dlfcn.h dl.h mach-o/dyld.h], [break])

dnl Checks for:
dnl   farcalloc (in bcc), 
dnl   valloc (in sunos, solaris, mips, amiga, next, minix, ultrix)
AC_CHECK_HEADER(alloc.h,[AC_CHECK_FUNCS(farcalloc)])
if test "${ac_cv_header_stdlib_h+set}" = set ||
   test "${ac_cv_header_unistd_h+set}" = set; then
  AC_CHECK_FUNCS(valloc)
fi

dnl--------------------------------------------------------------------
dnl Check for Unicode support, and select encoding
dnl--------------------------------------------------------------------

AC_CHECK_HEADERS(locale.h wchar.h)
if test "x$host_os" = "xmingw32"; then
  wchar_t_is_unicode=yes	# actually UTF-16
elif test "${ac_cv_header_wchar_h+set}" = set; then
  AC_MSG_CHECKING([whether wchar_t is ISO 10646 (Unicode)])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
    [[#include <wchar.h>
#ifndef __STDC_ISO_10646__
# error __STDC_ISO_10646__ not defined
#endif]])],
    [wchar_t_is_unicode=yes],
    [wchar_t_is_unicode=no])
  AC_MSG_RESULT([$wchar_t_is_unicode])
else
  wchar_t_is_unicode=no
fi

AC_MSG_CHECKING([for locale-based character encoding])
if test "${ac_cv_header_locale_h+set}" = set &&
   test "$wchar_t_is_unicode" = yes; then
  locale_char_encoding=yes
else
  locale_char_encoding=no
fi
AC_MSG_RESULT([$locale_char_encoding])

case $enable_char_encoding in
locale|yes)
  if test "$locale_char_encoding" = yes; then
    enable_char_encoding=locale
  else
    AC_MSG_ERROR([System does not support locale-based encoding of Unicode.])
    enable_char_encoding=no
  fi ;;
autodetect)
  if test "$locale_char_encoding" = yes; then
    enable_char_encoding=locale
  fi ;;
esac

case $enable_char_encoding in
locale)
  AC_DEFINE([CHAR_ENCODING_LOCALE], [1],
    [Define to 1 to use a Char encoding determined by the locale.]) ;;
utf8)
  AC_DEFINE([CHAR_ENCODING_UTF8], [1],
    [Define to 1 to use the UTF-8 Char encoding.]) ;;
esac

AC_SEARCH_LIBS([wcrtomb], [msvcp60])

dnl--------------------------------------------------------------------
dnl
dnl Check for library functions.
dnl
dnl--------------------------------------------------------------------

FP_CHECK_FUNC([WinExec],
  [@%:@include <windows.h>], [WinExec("",0)])

FP_CHECK_FUNC([GetModuleFileName],
  [@%:@include <windows.h>], [GetModuleFileName((HMODULE)0,(LPTSTR)0,0)])

AC_CHECK_FUNCS([atan],
  [AC_DEFINE([FLOATS_SUPPORTED], [1],
    [Define to 1 if floating point arithmetic is supported.])])

AC_CHECK_FUNCS(strcasecmp _stricmp stricmp strcmpi)
AC_CHECK_FUNCS(strcmp)
AC_CHECK_FUNCS(rindex)
AC_CHECK_FUNCS(strrchr)
AC_CHECK_FUNCS(realpath _fullpath)  
AC_CHECK_FUNCS(macsystem)  
AC_CHECK_FUNCS(fseek ftell)
AC_CHECK_FUNCS(vsnprintf _vsnprintf)
AC_CHECK_FUNCS(snprintf  _snprintf )  
AC_CHECK_FUNCS(popen     _popen )  
AC_CHECK_FUNCS(pclose    _pclose )  
AC_CHECK_FUNCS(sigprocmask)  
AC_CHECK_FUNCS(getrusage)
AC_CHECK_FUNCS(times)
AC_CHECK_FUNCS(isatty)
AC_CHECK_FUNCS(fstat)
AC_CHECK_FUNCS(select)
AC_CHECK_FUNCS(gettimeofday)
AC_CHECK_FUNCS(ftime)
AC_CHECK_FUNCS(time)
AC_CHECK_FUNCS(localtime)
AC_CHECK_FUNCS(gmtime)
AC_CHECK_FUNCS(mktime)
AC_CHECK_FUNCS(dup)

dnl--------------------------------------------------------------------
dnl
dnl Timezonery
dnl
dnl--------------------------------------------------------------------

AC_STRUCT_TIMEZONE
FP_DECL_TIMEZONE

dnl This always fails unless you also #include <windows.h>
dnl AC_CHECK_FUNCS(GetModuleFileName GetModuleFileNameA)      dnl Windows

dnl Probably Macintosh specific
dnl AC_CHECK_FUNCS(getfinfo)

AC_FUNC_ALLOCA   dnl Also sets STACK_DIRECTION

dnl Visual C++ doesn't have alloca, does have _alloca macro
AC_CACHE_CHECK(for _alloca, ac_cv_c__alloca,
[AC_TRY_LINK([
#include <malloc.h>
int test1() {
    return _alloca(42);
}
],
[int i;], 
ac_cv_c__alloca=yes,
ac_cv_c__alloca=no)])
if test "$ac_cv_c__alloca" = yes; then
AC_DEFINE(HAVE__ALLOCA, [1],
[Define to 1 if you have malloc.h and it defines _alloca - eg for Visual C++.])
fi

dnl Library functions used in C interface Foreign/test.ss (non-essential)

AC_CHECK_FUNCS(stime poly)

dnl--------------------------------------------------------------------
dnl
dnl Check for typedefs, structures, and compiler characteristics
dnl
dnl--------------------------------------------------------------------


AC_C_CONST               dnl can we use "const"?
AC_C_PROTOTYPES          dnl can we use function prototypes?
AC_C_JMPBUF_ARRAY        dnl can we take address of jmpbufs?
AC_C_LABELS_AS_VALUES    dnl can we use gcc's "labels as values" feature?

dnl AC_C_CHAR_UNSIGNED   dnl We can test for signed/unsigned chars - but don't use it

AC_STRUCT_TM

dnl We don't test for this because Borland C uses a typedef to define size_t
dnl but puts it in stdio.h - this conflicts with defining it in config.h
dnl AC_TYPE_SIZE_T

dnl Foo: assumes we can use prototypes.
dnl On BCC, signal handlers have type "int(void)", elsewhere its "void(int)".
dnl AC_CACHE_CHECK([type of signal handlers], ac_cv_type_signal_handler,
dnl [AC_TRY_COMPILE([#include <sys/types.h>
dnl #include <signal.h>
dnl #ifdef signal
dnl #undef signal
dnl #endif
dnl void (*signal (int, void (*)(int)))(int);
dnl ],
dnl [int i;], 
dnl ac_cv_type_signal_handler=void_int,
dnl ac_cv_type_signal_handler=int_void)])
dnl if test "$ac_cv_type_signal_handler" = void_int; then
dnl AC_DEFINE(VOID_INT_SIGNALS)
dnl fi

dnl On BCC, signal handlers have type "int(void)", elsewhere its "void(int)".
AC_TYPE_SIGNAL
if test "$ac_cv_type_signal" = void; then
AC_DEFINE(VOID_INT_SIGNALS, [1],
  [Define to 1 if signal handlers have type void (*)(int)
   (Otherwise, they're assumed to have type int (*)(void).)])
fi


dnl--------------------------------------------------------------------
dnl 
dnl Readline - based on the feature testing that the GHC configure
dnl            script performs.
dnl
dnl--------------------------------------------------------------------
AC_CHECK_LIB(ncurses, tputs, HaveLibTermcap=YES; LibTermcap=ncurses,
  AC_CHECK_LIB(termcap, tputs, HaveLibTermcap=YES; LibTermcap=termcap,
    AC_CHECK_LIB(curses, tputs, HaveLibTermcap=YES; LibTermcap=curses,
      HaveLibTermcap=NO)))
if test $HaveLibTermcap = YES ; then
    LIBS="-l$LibTermcap $LIBS"
fi

AC_CHECK_LIB(readline, readline, HaveLibReadline=YES; LibReadline=readline,
  AC_CHECK_LIB(editline, readline, HaveLibReadline=YES; LibReadline=editline,
    HaveLibReadline=NO))
if test $HaveLibTermcap = YES && test $HaveLibReadline = YES &&
   test "$enable_char_encoding" != utf8; then
    LIBS="-l$LibReadline $LIBS"
    AC_DEFINE([USE_READLINE], [1],
      [Define to 1 if a command line editor is available and should be
       used. There are two choices of command line editor that can be
       used with Hugs: GNU readline and editline (from comp.sources.misc,
       vol 31, issue 71)])
fi

dnl--------------------------------------------------------------------
dnl
dnl Profiling
dnl
dnl--------------------------------------------------------------------
if test "$enable_profiling" = yes; then
    if test "$have_hp2ps" = "0"; then
        AC_MSG_WARN(
[hp2ps (heap profile display program) not available 
(not ignoring --enable-profiling)])
    fi
fi

dnl--------------------------------------------------------------------
dnl
dnl Timer
dnl
dnl--------------------------------------------------------------------
want_timer=no
if test "$enable_timer" = yes; then
    if test "$ac_cv_header_time_h" = yes; then
        want_timer=yes
    elif test "$ac_cv_header_sys_time_h" = yes &&
	 test "$ac_cv_header_sys_resource_h" = yes; then
        want_timer=yes
    else
        AC_MSG_WARN(
[neither <time.h> nor (<sys/time.h> and <sys/resource.h>) is available
(ignoring --enable-timer)])
    fi
fi
if test "$want_timer" = yes; then
  AC_DEFINE([WANT_TIMER], [1],
   [Define if you want to time every evaluation. Timing is included in
    the Hugs distribution for the purpose of benchmarking the Hugs
    interpreter, comparing its performance across a variety of different
    machines, and with other systems for similar languages. It would be
    somewhat foolish to try to use the timings produced in this way for
    any other purpose.  In particular, using timings to compare the
    performance of different versions of an algorithm is likely to give
    very misleading results.  The current implementation of Hugs as an
    interpreter, without any significant optimizations, means that there
    are much more significant overheads than can be accounted for by
    small variations in Hugs code.])
fi

dnl--------------------------------------------------------------------
dnl
dnl Debugging flags (assumed to be mutually exclusive with optimisation)
dnl
dnl--------------------------------------------------------------------
if test "$enable_debug" = yes; then
    OPTFLAGS=""
else
    CFLAGS="-DNDEBUG=1 $CFLAGS"
    DEBUGFLAGS=""
    LDDEBUGFLAGS=""
fi
AC_SUBST(OPTFLAGS)
AC_SUBST(DEBUGFLAGS)
AC_SUBST(LDDEBUGFLAGS)

dnl--------------------------------------------------------------------
dnl
dnl Figure out sizes of objects and decide which version of Hugs to build
dnl
dnl--------------------------------------------------------------------

AC_CHECK_SIZEOF(int,4)
AC_CHECK_SIZEOF(float,4)
AC_CHECK_SIZEOF(double,8)
AC_CHECK_SIZEOF(int*,4)

if test "$ac_cv_sizeof_int" -eq "2"; then
    AC_DEFINE([SMALL_HUGS], [1],
      [Define to 1 for 16 bit operation on a limited memory PC.])
else
    dnl ToDo: test for virtual memory - if you have it, LARGE_HUGS is cool
    dnl also: provide a way to override default
    if test 0 -eq 1; then
        echo "building regular hugs"
        AC_DEFINE([REGULAR_HUGS], [1],
          [Define to 1 for 32 bit operation using largish default table sizes.])
    else
        echo "building large hugs"
        AC_DEFINE([LARGE_HUGS], [1],
          [Define to 1 for 32 bit operation using larger default table sizes.])
    fi
fi


dnl--------------------------------------------------------------------
dnl
dnl Figure out how to do dynamic linking.
dnl
dnl It is fairly easy to do the dynamic loading: that is fairly well 
dnl documented and only depends on the host platform.
dnl 
dnl It is also fairly easy to decide whether or not to add a leading 
dnl underscore to symbol names when doing symbol lookup.
dnl
dnl But it is bloody hard to figure out how to _build_ a file which can be
dnl dynamically loaded because it varies with both the platform and the
dnl choice of linker.  So we use a rather crude approach: we try every
dnl set of linking commands that anyone suggests will work.
dnl
dnl At the time of writing, I have no idea which of these tests will work.
dnl I don't want to add flags that are obviously silly but I want to
dnl record what is worth trying.  So, for now, I'm going to leave all
dnl the untested ones commented out.  As platforms are found which need
dnl those flags, we'll uncomment each line (and maybe even record which
dnl platforms those are).
dnl
dnl--------------------------------------------------------------------

dnl Try the MS Visual C flags
HUGS_TRY_DYNLINK([/LD])
HUGS_TRY_DYNLINK([/LD /ML /nologo])

dnl Try the win32 way
dnl gcc-mingw, gcc-cygwin don't want to be given -fPIC,
dnl but this test fouls up gcc-linux on some architectures.
case "$host_os" in
mingw32|cygwin32)
  HUGS_TRY_DYNLINK([-shared]) ;;
esac

dnl Try the gcc flags
HUGS_TRY_DYNLINK([-shared -fPIC])

dnl Try the MacOS X flags
HUGS_TRY_DYNLINK([-flat_namespace -bundle -undefined suppress])

dnl Try the MacOS X flags
HUGS_TRY_DYNLINK([-bundle])        

dnl Try the MacOS X flags
dnl HUGS_TRY_DYNLINK([-bundle -lc])        

dnl Try the gcc flags
dnl HUGS_TRY_DYNLINK([-shared -nostdlib])

dnl Some other gcc flags
HUGS_TRY_DYNLINK(-r)

dnl Some other gcc flags
dnl HUGS_TRY_DYNLINK(-rdynamic)

dnl Try the SunOS flags
dnl HUGS_TRY_DYNLINK(-G)

dnl Try the HPUX flags
dnl HUGS_TRY_DYNLINK([-Aa +z])             

AC_DEFINE_UNQUOTED(MKDLL_CMD,
  ["${CC-cc} $CFLAGS $CPPFLAGS $LDFLAGS $ac_cv_dll_flags"],
  [C compiler invocation use to build a dynamically loadable library.
   Typical value: "gcc -shared".  Must evaluate to a literal C string.])
if test "$ac_cv_leading_underscore" = "yes"; then
AC_DEFINE(LEADING_UNDERSCORE, [1],
  [Define to 1 if your C compiler inserts underscores before symbol names.])
fi

dynamic_loading=no
FP_CHECK_FUNC([LoadLibrary],
  [@%:@include <windows.h>], [LoadLibrary("")],
  [dynamic_loading=yes],
  [AC_CHECK_FUNCS([dlopen shl_load NSCreateObjectFileImageFromFile],
    [dynamic_loading=yes; break])])

if test x"$enable_ffi" = xautodetect; then
  enable_ffi="$dynamic_loading"
fi

if test x"$enable_ffi" = xyes; then
  FFI_LIBRARIES=ffi_libraries
else
  FFI_LIBRARIES=
fi
AC_SUBST([FFI_LIBRARIES])

dnl--------------------------------------------------------------------
dnl
dnl Lint flags
dnl (placed after DLL stuff, so not included in MKDLL_CMD)
dnl
dnl--------------------------------------------------------------------

dnl These flags are gcc specific.  
dnl They detecting many common programming errors but can get fooled
dnl by perfectly safe code so we usually turn them off.
dnl We don't use -Wstrict-prototypes because some definitions in 
dnl storage.h tickle this flag and can't be rewritten the 'correct' way
dnl without generating a warning/error from Microsoft's compiler.
GCC_LINT_FLAGS='-Wall -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wmissing-prototypes -Wmissing-declarations -Wnested-externs -Wno-parentheses'

if test "$enable_lint" = yes; then
    case "$CC" in
    gcc|gcc-*|*/gcc|*/gcc-*)
        LINT_FLAGS="$GCC_LINT_FLAGS" ;;
#    bcc32)
#        LINT_FLAGS="$BCC32_LINT_FLAGS" ;;
    *)
        AC_MSG_WARN(dnl
[lint flags only defined for gcc (at present)
(ignoring --enable-lint)]) ;;
    esac

    CFLAGS="$CFLAGS $LINT_FLAGS"
    dnl AC_MSG_WARN(using lint $CFLAGS)
fi

dnl--------------------------------------------------------------------
dnl
dnl Architecture specific substitutions
dnl The default values can be overridden by environment variables when
dnl the configure script is called
dnl
dnl--------------------------------------------------------------------

dnl When building a Windows script, $host_os will be set to "mingw32".

if test "x$host_os" = "xmingw32"; then
    BAT=.bat
    DLL=".dll"          
    DLL_ENDING="-dll"   
    DEV_NULL="nul"
    hugsdir_deflt='${libdir}/hugs'
    HUGSPATH=".;{Hugs}\\\\packages\\\\*"
    HUGSSUFFIXES=".hs;.lhs"
    MkInstall="src/MkInstal.in"
elif test "x$host" = "xdos"; then
    # Much the same as for Windows
    BAT=.bat
    DLL=".dll"          
    DLL_ENDING="-dll"   
    DEV_NULL="nul"
    hugsdir_deflt='${libdir}\\hugs'
    HUGSPATH=".;{Hugs}\\\\packages\\\\*"
    HUGSSUFFIXES=".hs;.lhs"
    MkInstall="src/MkNull.in"
else
    BAT=
    hugsdir_deflt='${libdir}/hugs'
    HUGSPATH=".:{Home}/lib/hugs/packages/*:/usr/local/lib/hugs/packages/*:{Hugs}/packages/*"
    HUGSSUFFIXES=".hs:.lhs"
    MkInstall="src/MkInstal.in"
fi
AC_SUBST(HUGSPATH)
AC_SUBST(HUGSSUFFIXES)
AC_DEFINE_UNQUOTED([HUGSSUFFIXES], ["$HUGSSUFFIXES"],
  [The list of suffixes used by Haskell source files, separated either
   by colons (Unix) or semicolons (Macintosh, Windows, DOS). This value
   can be overridden using the -S flag.])

if test "x$prefix" = xNONE; then
    prefix="$ac_default_prefix"
fi

AC_SUBST_DEF(hugsdir,"$hugsdir_deflt")

test "x$prefix"      = xNONE && prefix=$ac_default_prefix
test "x$exec_prefix" = xNONE && exec_prefix='${prefix}'

dnl This is the only way I could find to perform the substitutions
dnl in hugsdir.  Sigh.
HUGSDIR=`sh -c "prefix=$prefix; exec_prefix=$exec_prefix; libdir=$libdir; datadir=$datadir; echo $hugsdir"`
AC_DEFINE_UNQUOTED([HUGSDIR], ["$HUGSDIR"],
  [The directory name which is substituted for the string "{Hugs}" in a
   path variable. This normally points to where the Hugs libraries are
   installed - ie so that the file HUGSDIR/packages/base/Prelude.hs exists.
   Typical values are: "/usr/local/lib/hugs", "/usr/homes/JFHaskell/hugs",
   "../hugsdir". This value is ignored on Windows and old MacOS versions
   since it is assumed that the binary is installed in HUGSDIR.
   This value can be overridden using the environment variable HUGSDIR,
   and you can always choose _not_ to use the {Hugs} variable.])
AC_SUBST(HUGSDIR)
BINDIR=`sh -c "prefix=$prefix; exec_prefix=$exec_prefix; echo $bindir"`
AC_SUBST(BINDIR)

AC_SUBST_DEF(DLL,".so")
AC_SUBST_DEF(DLL_ENDING,"-so")
AC_SUBST_DEF(DEV_NULL,"/dev/null")
AC_SUBST_DEF(RM,"/bin/rm -f")
AC_SUBST_DEF(CP,"/bin/cp")
AC_SUBST(BAT)

AC_SUBST_FILE(MkInstall)
MkDepend=src/MkDepend.in
AC_SUBST_FILE(MkDepend)  

AC_DEFINE_UNQUOTED([HUGSPATH], ["${HUGSPATH}"],
  [Define this as the default setting of HUGSPATH. Value may contain
   string "{Hugs}" (for which we will substitute the value of HUGSDIR)
   and should be either colon-separated (Unix) or semicolon-separated
   (Macintosh, Windows, DOS). Escape characters in the path string are
   interpreted according to normal Haskell conventions. This value can be
   overridden from the command line by setting the HUGSFLAGS environment
   variable or by storing an appropriate value for HUGSFLAGS in the
   registry (Win32 only).  In all cases, use a string of the form
   -P"...".])

dnl--------------------------------------------------------------------
dnl
dnl Figure out whether we can build the GUI
dnl (done last because -W flags messes everything else up)
dnl
dnl--------------------------------------------------------------------

HUGS_FOR_WINDOWS=no
if test "x$with_gui" = xyes; then
  if test "x$host_os" = "xmingw32"; then
    LDFLAGS="$LDFLAGS -W"
    WOBJECTS="win-text.obj"
    STRIP="brc32 hugs32.rc hugs.exe -w32"
    RC_FILES="hugs32.rc"
    HUGS_FOR_WINDOWS=yes
  elif test "x$host" = "xdos"; then
    WOBJECTS="win-text.obj"
    STRIP="brc hugs16.rc hugs.exe"
    RC_FILES="hugs16.rc"
    HUGS_FOR_WINDOWS=yes
  else
    AC_MSG_WARN(
[not building Hugs GUI because this isn't a DOS machine
    ])
  fi
fi
if test $HUGS_FOR_WINDOWS = yes; then
  AC_DEFINE([HUGS_FOR_WINDOWS], [1],
    [Define to 1 if you want to use the "Hugs for Windows" GUI.
     (Windows 3.1 and compatibles only)])
fi
AC_SUBST_DEF(WOBJECTS,"")
AC_SUBST_DEF(RC_FILES,"")
AC_SUBST_DEF(STRIP,"")

dnl--------------------------------------------------------------------
dnl
dnl Generate output files
dnl
dnl--------------------------------------------------------------------

TESTSCRIPT=""
if test -f tests/config.in ; then
 TESTSCRIPT=tests/config
fi

MAKEFILES='src/Makefile docs/Makefile docs/users_guide/Makefile demos/Makefile'
AC_OUTPUT(MkDefs $MAKEFILES src/platform.h docs/hugs.1 $TESTSCRIPT)

echo ''
echo '************************************************'
echo '*** NOW DO: make ; make install'
echo '************************************************'
exit 0

dnl End of configure.ac
