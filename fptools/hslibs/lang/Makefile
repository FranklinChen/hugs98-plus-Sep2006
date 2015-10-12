# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.22 2004/11/26 16:22:00 simonmar Exp $
#
# (c) The University of Glasgow 2002
#

TOP=..
include $(TOP)/mk/boilerplate.mk

PACKAGE  = lang
VERSION = 1.0
PACKAGE_DEPS = mtl
SUBDIRS  = cbits
ALL_DIRS = monads

SRC_HC_OPTS += -cpp -fglasgow-exts -Icbits

include $(TOP)/mk/target.mk
