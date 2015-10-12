# -----------------------------------------------------------------------------

TOP = ..
include $(TOP)/mk/boilerplate.mk
-include config.mk

ifneq "$(findstring clean, $(MAKECMDGOALS))" ""
# if we're cleaning, then config.mk might have been cleaned already
ALUT_BUILD_PACKAGE=yes
PACKAGE=ALUT
endif

ifneq "$(ALUT_BUILD_PACKAGE)" "no"

# -----------------------------------------------------------------------------

SUBDIRS = include

ifeq "$(IncludeExampleDirsInBuild)" "YES"
SUBDIRS += examples
endif

ALL_DIRS = Sound Sound/ALUT

PACKAGE_DEPS = base OpenGL OpenAL

SRC_HC_OPTS += -Wall -fffi -Iinclude '-\#include "HsALUT.h"' -cpp

# WinDoze DLL hell
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SRC_HC_OPTS += -DCALLCONV=stdcall
else
SRC_HC_OPTS += -DCALLCONV=ccall
endif

PACKAGE_CPP_OPTS += -DMAINTAINER=$(MAINTAINER)

SRC_HADDOCK_OPTS += -t "HOpenAL Libraries (ALUT package)"

# -----------------------------------------------------------------------------

package.conf.inplace \
package.conf.installed \
Sound/ALUT/Constants.$(way_)o : include/HsALUTConfig.h

endif

EXCLUDED_SRCS += Setup.hs

# -----------------------------------------------------------------------------

DIST_CLEAN_FILES += ALUT.buildinfo config.cache config.status config.mk

extraclean::
	$(RM) -rf autom4te.cache

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
