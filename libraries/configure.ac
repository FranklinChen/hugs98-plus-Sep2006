dnl--------------------------------------------------------------------
dnl
dnl Hugs98 libraries configure script template.
dnl Process ../configure.ac with autoreconf to produce a configure script.
dnl
dnl--------------------------------------------------------------------

AC_INIT([Hugs98 libraries], [1.0], [hugs-bugs@haskell.org], [hugslibs])
AC_CONFIG_SRCDIR([include/MachDeps.h])

dnl We need 2.50 for AC_HELP_STRING.
AC_PREREQ([2.50])

dnl Named to match the fptools configuration header file
AC_CONFIG_HEADER(include/ghcconfig.h)
AH_TOP([/* platform-specific defines */
#include "platform.h"])

# necessary evil
AC_CONFIG_COMMANDS([ultra-evil], [echo '
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION' >>include/ghcconfig.h])

dnl --------------------------------------------------
dnl ### program checking ###
dnl --------------------------------------------------

AC_PROG_CC
AC_PROG_GCC_TRADITIONAL

dnl ** figure out how to invoke cpp directly (gcc -E is no good)
AC_PROG_CPP
ICE_PROG_CPP_TRADITIONAL

AC_PROG_MAKE_SET

dnl Look for right versions of 'find' and 'sort' (win32 only, really.)
FP_PROG_FIND
FP_PROG_SORT

dnl--------------------------------------------------------------------
dnl ### types 
dnl--------------------------------------------------------------------

dnl ** do we have long longs?
AC_CHECK_TYPES([long long])

dnl ** what are the sizes of various types
AC_CHECK_SIZEOF(char,               1)
AC_CHECK_SIZEOF(double,             8)
AC_CHECK_SIZEOF(float,              4)
AC_CHECK_SIZEOF(int,                4)
AC_CHECK_SIZEOF(long,               4)
if test "$ac_cv_type_long_long" = yes; then
AC_CHECK_SIZEOF(long long,          8)
fi
AC_CHECK_SIZEOF(short,              2)
AC_CHECK_SIZEOF(unsigned char,      1)
AC_CHECK_SIZEOF(unsigned int,       4)
AC_CHECK_SIZEOF(unsigned long,      4)
if test "$ac_cv_type_long_long" = yes; then
AC_CHECK_SIZEOF(unsigned long long, 8)
fi
AC_CHECK_SIZEOF(unsigned short,     2)
AC_CHECK_SIZEOF(void *,             4)

dnl ** what are alignment constraints on various types
FP_CHECK_ALIGNMENT(char)
FP_CHECK_ALIGNMENT(double)
FP_CHECK_ALIGNMENT(float)
FP_CHECK_ALIGNMENT(int)
FP_CHECK_ALIGNMENT(long)
if test "$ac_cv_type_long_long" = yes; then
FP_CHECK_ALIGNMENT(long long)
fi
FP_CHECK_ALIGNMENT(short)
FP_CHECK_ALIGNMENT(unsigned char)
FP_CHECK_ALIGNMENT(unsigned int)
FP_CHECK_ALIGNMENT(unsigned long)
if test "$ac_cv_type_long_long" = yes; then
FP_CHECK_ALIGNMENT(unsigned long long)
fi
FP_CHECK_ALIGNMENT(unsigned short)
FP_CHECK_ALIGNMENT(void *)

dnl Definitions computed by ../configure, for insertion into Makefile
MkDefs=../MkDefs
AC_SUBST_FILE(MkDefs)  

AC_CONFIG_FILES(Makefile tools/config)

AC_OUTPUT
