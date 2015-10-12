# -----------------------------------------------------------------------------

TOP = ..
include $(TOP)/mk/boilerplate.mk
-include config.mk

ifneq "$(findstring clean, $(MAKECMDGOALS))" ""
# if we're cleaning, then config.mk might have been cleaned already
AL_BUILD_PACKAGE=yes
PACKAGE=OpenAL
endif

ifneq "$(AL_BUILD_PACKAGE)" "no"

# -----------------------------------------------------------------------------

SUBDIRS = include

ifeq "$(IncludeExampleDirsInBuild)" "YES"
SUBDIRS += examples
endif

ALL_DIRS = \
	Sound \
	Sound/OpenAL \
	Sound/OpenAL/AL \
	Sound/OpenAL/ALC

PACKAGE_DEPS = base OpenGL

SRC_HC_OPTS += -Wall -fffi -Iinclude '-\#include "HsOpenAL.h"' -cpp

# WinDoze DLL hell
# NOTE: For some obscure/unknown reason, OpenAL uses ccall, even on WinDoze...
SRC_HC_OPTS += -DCALLCONV=ccall
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SRC_HC_OPTS := $(subst -mno-cygwin,,$(SRC_HC_OPTS))
endif

PACKAGE_CPP_OPTS += -DMAINTAINER=$(MAINTAINER)

SRC_HADDOCK_OPTS += -t "HOpenAL Libraries (OpenAL package)" -p prologue.txt

# -----------------------------------------------------------------------------

package.conf.inplace \
package.conf.installed \
Sound/OpenAL/Config.$(way_)o \
Sound/OpenAL/Constants.$(way_)o : include/HsOpenALConfig.h

endif

EXCLUDED_SRCS += Setup.hs

# -----------------------------------------------------------------------------

DIST_CLEAN_FILES += OpenAL.buildinfo config.cache config.status config.mk

extraclean::
	$(RM) -rf autom4te.cache

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
