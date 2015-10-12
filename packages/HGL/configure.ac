AC_INIT([Haskell Graphics Library], [3.1], [libraries@haskell.org], [HGL])

FP_ARG_HGL

if test "$enable_hgl" = no; then
  HGL_BUILD_PACKAGE=no
else
# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([Graphics/HGL.hs])

AC_CONFIG_HEADERS([include/HsHGLConfig.h])

# Check for X11 include paths and libraries
AC_PATH_XTRA

# AC_PATH_XTRA doesn't actually check that the C compiler can
# really include the X headers, so double-check here.  In particular,
# this will catch the case of using a mingw32 gcc on a Cygwin system:
# Cygwin has the X headers & libs installed, but the mingw32 gcc can't
# use them, and we want to disable the package in this case.
CPPFLAGS="$CPPFLAGS $X_CFLAGS"
AC_TRY_CPP([#include <X11/Xlib.h>],,[no_x=yes])

# Build the package if we found X11 stuff
if test "$no_x" = yes; then
  # or we're on Windows
  AC_CHECK_HEADER([windows.h],
    [HGL_BUILD_PACKAGE=yes
     PLATFORM=Win32
     AC_DEFINE(X_DISPLAY_MISSING)],
    [HGL_BUILD_PACKAGE=no])
else
  HGL_BUILD_PACKAGE=yes
  PLATFORM=X11
fi
fi
AC_SUBST([HGL_BUILD_PACKAGE])
AC_SUBST([PLATFORM])

case "$PLATFORM" in
Win32)	EXTRA_MODULES='Graphics.HGL.Win32.Bitmap, Graphics.HGL.Win32.Draw, Graphics.HGL.Win32.Types, Graphics.HGL.Win32.WND' ;;
X11)	EXTRA_MODULES='Graphics.HGL.X11.Display, Graphics.HGL.X11.DC, Graphics.HGL.X11.Timer, Graphics.HGL.X11.Types, Graphics.HGL.X11.Window' ;;
*)	EXTRA_MODULES= ;;
esac
AC_SUBST([EXTRA_MODULES])

# Define CPP variables used in package.conf.in
if test "$HGL_BUILD_PACKAGE" = yes; then
  AC_DEFINE_UNQUOTED([PLATFORM], [$PLATFORM],
	[Package providing the underlying graphics library.])
fi

if test "$HGL_BUILD_PACKAGE" = yes; then
  BUILD_PACKAGE_BOOL=True
else
  BUILD_PACKAGE_BOOL=False
fi
AC_SUBST([BUILD_PACKAGE_BOOL])

AC_CONFIG_FILES([config.mk HGL.buildinfo])
AC_OUTPUT
