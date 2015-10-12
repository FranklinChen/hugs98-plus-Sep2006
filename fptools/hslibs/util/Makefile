# -----------------------------------------------------------------------------
#
# (c) The University of Glasgow 2002
#

TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS  = cbits
ALL_DIRS = check
PACKAGE      = util
VERSION = 1.0
PACKAGE_DEPS = lang concurrent

# We omit the Readline library on mingw32, because it can't be loaded
# into GHCi due to the lack of a readline DLL, and that prevents the
# util package from being loadable in GHCi too.  On Windows you can still
# get the Readline library from the readline package (System.Console.Readline).
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
EXCLUDED_SRCS += Readline.hs
else
ifeq "$(GhcLibsWithReadline)" "YES"
PACKAGE_DEPS += readline
else
EXCLUDED_SRCS += Readline.hs
endif
endif

SRC_HC_OPTS += -cpp

SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR)

Memo_HC_OPTS += -funbox-strict-fields

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
  EXCLUDED_SRCS += Select.lhs
  PACKAGE_DEPS := $(filter-out posix, $(PACKAGE_DEPS))
endif

include $(TOP)/mk/target.mk
