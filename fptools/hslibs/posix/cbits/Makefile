# $Id: Makefile,v 1.18 2005/08/10 07:57:33 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_CC_OPTS += -Wall
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_RUNTIME_DIR)

LIBRARY = libHSposix_cbits.a
LIBOBJS = $(C_OBJS)

includedir = $(libdir)/include
INSTALL_INCLUDES = HsPosix.h

include $(TOP)/mk/target.mk
