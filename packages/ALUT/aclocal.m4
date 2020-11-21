# FP_COMPUTE_INT(EXPRESSION, VARIABLE, INCLUDES, IF-FAILS)
# ---------------------------------------------------------
# Assign VARIABLE the value of the compile-time EXPRESSION using INCLUDES for
# compilation. Execute IF-FAILS when unable to determine the value. Works for
# cross-compilation, too.
#
# Implementation note: We are lazy and use an internal autoconf macro, but it
# is supported in autoconf versions 2.50 up to the actual 2.57, so there is
# little risk.
AC_DEFUN([FP_COMPUTE_INT],
[_AC_COMPUTE_INT([$1], [$2], [$3], [$4])[]dnl
])# FP_COMPUTE_INT


# FP_CHECK_CONST(EXPRESSION, [INCLUDES = DEFAULT-INCLUDES], [VALUE-IF-FAIL = -1])
# -------------------------------------------------------------------------------
# Defines CONST_EXPRESSION to the value of the compile-time EXPRESSION, using
# INCLUDES. If the value cannot be determined, use VALUE-IF-FAIL.
AC_DEFUN([FP_CHECK_CONST],
[AS_VAR_PUSHDEF([fp_Cache], [fp_cv_const_$1])[]dnl
AC_CACHE_CHECK([value of $1], fp_Cache,
[FP_COMPUTE_INT([$1], fp_check_const_result, [AC_INCLUDES_DEFAULT([$2])],
                [fp_check_const_result=m4_default([$3], ['-1'])])
AS_VAR_SET(fp_Cache, [$fp_check_const_result])])[]dnl
AC_DEFINE_UNQUOTED(AS_TR_CPP([CONST_$1]), AS_VAR_GET(fp_Cache), [The value of $1.])[]dnl
AS_VAR_POPDEF([fp_Cache])[]dnl
])# FP_CHECK_CONST


# FP_CHECK_CONSTS_TEMPLATE(EXPRESSION...)
# ---------------------------------------
# autoheader helper for FP_CHECK_CONSTS
m4_define([FP_CHECK_CONSTS_TEMPLATE],
[AC_FOREACH([fp_Const], [$1],
  [AH_TEMPLATE(AS_TR_CPP(CONST_[]fp_Const),
               [The value of ]fp_Const[.])])[]dnl
])# FP_CHECK_CONSTS_TEMPLATE


# FP_CHECK_CONSTS(EXPRESSION..., [INCLUDES = DEFAULT-INCLUDES], [VALUE-IF-FAIL = -1])
# -----------------------------------------------------------------------------------
# List version of FP_CHECK_CONST
AC_DEFUN([FP_CHECK_CONSTS],
[FP_CHECK_CONSTS_TEMPLATE([$1])dnl
for fp_const_name in $1
do
FP_CHECK_CONST([$fp_const_name], [$2], [$3])
done
])# FP_CHECK_CONSTS

# FP_ARG_OPENAL
# -------------
AC_DEFUN([FP_ARG_OPENAL],
[AC_ARG_ENABLE([openal],
  [AC_HELP_STRING([--enable-openal],
    [build a Haskell binding for OpenAL (default=autodetect)])],
  [enable_openal=$enableval],
  [enable_openal=yes])
])# FP_ARG_OPENAL


# FP_ARG_ALUT
# -------------
AC_DEFUN([FP_ARG_ALUT],
[AC_ARG_ENABLE([alut],
  [AC_HELP_STRING([--enable-alut],
    [build a Haskell binding for ALUT (default=autodetect)])],
  [enable_alut=$enableval],
  [enable_alut=yes])
])# FP_ARG_ALUT


# FP_CHECK_ALUT
# -------------
AC_DEFUN([FP_CHECK_ALUT],
[AC_REQUIRE([AC_CANONICAL_TARGET])
ALUT_CFLAGS=
case $target_os in
darwin*)
  ALUT_LIBS=-lalut
  ALUT_FRAMEWORKS=
  ;;
*)
  AL_LIBS=no
  AC_SEARCH_LIBS([alGenSources], [openal openal32], [AL_LIBS="$ac_cv_search_alGenSources"])
  test x"$AL_LIBS" = x"none required" && AL_LIBS=

  fp_save_libs="$LIBS"
  LIBS="$AL_LIBS $LIBS"
  AC_SEARCH_LIBS([alutExit], [alut alut32], [ALUT_LIBS="$ac_cv_search_alutExit"])
  test x"$ALUT_LIBS" = x"none required" && ALUT_LIBS=
  LIBS="$fp_save_libs"
  ALUT_FRAMEWORKS=
  ;;
esac
AC_SUBST([ALUT_CFLAGS])
AC_SUBST([ALUT_LIBS])
AC_SUBST([ALUT_FRAMEWORKS])
])# FP_CHECK_ALUT


# FP_HEADER_ALUT
# --------------
# Check for an ALUT header, setting the variable fp_found_alut_header to no/yes,
# depending on the outcome.
AC_DEFUN([FP_HEADER_ALUT],
[if test -z "$fp_found_alut_header"; then
  fp_found_alut_header=no
  AC_CHECK_HEADERS([AL/alut.h OpenAL/alut.h], [fp_found_alut_header=yes; break])
fi
]) # FP_HEADER_AL


# FP_CHECK_FUNC_ALUT(FUNCTION, ARGUMENTS)
# ---------------------------------------
AC_DEFUN([FP_CHECK_FUNC_ALUT],
[AS_VAR_PUSHDEF([alut_var], [alut_cv_func_$1])dnl
AC_CACHE_CHECK([for $1],
  alut_var,
  [AC_LINK_IFELSE([AC_LANG_PROGRAM([[#if HAVE_AL_ALUT_H
#include <AL/alut.h>
#elif HAVE_OPENAL_ALUT_H
#include <OpenAL/alut.h>
#endif]],
                    [$1 $2;])],
                  [AS_VAR_SET(alut_var, yes)],
                  [AS_VAR_SET(alut_var, no)])])
if test AS_VAR_GET(alut_var) = yes; then
  AC_DEFINE_UNQUOTED(AS_TR_CPP([HAVE_$1]), [1],
                    [Define to 1 if you have the `$1' function.])
fi
AS_VAR_POPDEF([alut_var])dnl
])# FP_CHECK_FUNC_ALUT


# FP_FUNC_ALUTINIT_VOID
# ---------------------
# Defines ALUTINIT_VOID to 1 if `alutInit' returns void.
AC_DEFUN([FP_FUNC_ALUTINIT_VOID],
[AC_REQUIRE([FP_HEADER_ALUT])
AC_CACHE_CHECK([whether alutInit returns void],
  [fp_cv_func_alutInit_void],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT
#if HAVE_AL_ALUT_H
#include <AL/alut.h>
#elif HAVE_OPENAL_ALUT_H
#include <OpenAL/alut.h>
#endif
],
 [[int x = (int)alutInit((int *)0, (char **)0);]])],
 [fp_cv_func_alutInit_void=no],
 [fp_cv_func_alutInit_void=yes])])
if test x"$fp_cv_func_alutInit_void" = xyes; then
  AC_DEFINE([ALUTINIT_VOID], [1], [Define to 1 if `alutInit' returns void.])
fi
]) # FP_FUNC_ALUTINIT_VOID


# FP_FUNC_ALUTEXIT_VOID
# ---------------------
# Defines ALUTEXIT_VOID to 1 if `alutExit' returns void.
AC_DEFUN([FP_FUNC_ALUTEXIT_VOID],
[AC_REQUIRE([FP_HEADER_ALUT])
AC_CACHE_CHECK([whether alutExit returns void],
  [fp_cv_func_alutExit_void],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT
#if HAVE_AL_ALUT_H
#include <AL/alut.h>
#elif HAVE_OPENAL_ALUT_H
#include <OpenAL/alut.h>
#endif
],
 [[int x = (int)alutExit();]])],
 [fp_cv_func_alutExit_void=no],
 [fp_cv_func_alutExit_void=yes])])
if test x"$fp_cv_func_alutExit_void" = xyes; then
  AC_DEFINE([ALUTEXIT_VOID], [1], [Define to 1 if `alutExit' returns void.])
fi
]) # FP_FUNC_ALUTEXIT_VOID
