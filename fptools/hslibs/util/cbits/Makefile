# $Id: Makefile,v 1.21 2004/11/12 14:07:03 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
EXCLUDED_SRCS += selectFrom.c
endif

SRC_CC_OPTS += -Wall
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_RUNTIME_DIR) \
		-I$(FPTOOLS_TOP)/libraries/base/include

# No files reqd for mingw any longer; a defined, but empty C_OBJS confuses
# the mk/ setup.
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
LIBRARY = libHSutil_cbits.a
LIBOBJS = $(C_OBJS)
endif

includedir = $(libdir)/include
INSTALL_INCLUDES = HsUtil.h

include $(TOP)/mk/target.mk
