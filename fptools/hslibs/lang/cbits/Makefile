# $Id: Makefile,v 1.14 2004/11/12 14:05:57 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_CC_OPTS += -Wall -DCOMPILING_STDLIB 
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_RUNTIME_DIR) \
		-I$(FPTOOLS_TOP)/libraries/base/include

LIBRARY = libHSlang_cbits.a
LIBOBJS = $(C_OBJS)

includedir = $(libdir)/include
INSTALL_INCLUDES = HsLang.h

include $(TOP)/mk/target.mk
