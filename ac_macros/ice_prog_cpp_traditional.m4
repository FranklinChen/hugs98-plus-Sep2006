dnl <http://cvs.sourceforge.net/viewcvs.py/ddd/ddd/acinclude.m4>
dnl This file is part of DDD.
dnl
dnl DDD is free software; you can redistribute it and/or
dnl modify it under the terms of the GNU General Public
dnl License as published by the Free Software Foundation; either
dnl version 2 of the License, or (at your option) any later version.
dnl
dnl ICE_PROG_CPP_TRADITIONAL
dnl ------------------------
dnl
dnl Set output variable `CPP_TRADITIONAL' to a command that runs a
dnl "traditional" C preprocessor (that is, pre-ANSI-C).
dnl Try each one of `$CPP', `$CC -E', `/lib/cpp' either without flags
dnl or with `-traditional-cpp' or with `-traditional'.
dnl
dnl Local changes:
dnl	- the test input is Haskellized
dnl	- added -Xs (for Solaris)
dnl
AC_DEFUN([ICE_PROG_CPP_TRADITIONAL],
[
AC_REQUIRE([AC_PROG_CPP])
AC_CACHE_CHECK([for a traditional C preprocessor],
[ice_cv_traditional_cpp],
[
cat > conftest.c << EOF
#if 1
{-# INLINE f'
 #-}
f' x = x+1
#endif
EOF
ice_cv_traditional_cpp=
ice_save_cpp="$CPP"
ice_save_cppflags="$CPPFLAGS"
for ice_cpp in "$CPP" "$CC -E" "/lib/cpp"; do
for ice_cppflags in '' ' -traditional-cpp' ' -traditional' ' -Xs'; do
CPP="$ice_cpp"
CPPFLAGS="$ice_cppflags"
AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#if 1
{-# INLINE f'
 #-}
f' x = x+1
#endif
]])], [ice_cv_traditional_cpp="${CPP}${CPPFLAGS}"])
if test "$ice_cv_traditional_cpp" != ""; then
break 2
fi
done
done
CPP="$ice_save_cpp"
CPPFLAGS="$ice_save_cppflags"
])
CPP_TRADITIONAL="$ice_cv_traditional_cpp"
AC_SUBST(CPP_TRADITIONAL)
])dnl
