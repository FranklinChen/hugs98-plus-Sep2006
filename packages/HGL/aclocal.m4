# Empty file to avoid a dependency on automake: autoreconf calls aclocal to
# generate a temporary aclocal.m4t when no aclocal.m4 is present.

# FP_ARG_HGL
# -------------
AC_DEFUN([FP_ARG_HGL],
[AC_ARG_ENABLE([hgl],
  [AC_HELP_STRING([--enable-hgl],
    [build HGL.
     (default=autodetect)])],
  [enable_hgl=$enableval],
  [enable_hgl=yes])
])# FP_ARG_HGL
