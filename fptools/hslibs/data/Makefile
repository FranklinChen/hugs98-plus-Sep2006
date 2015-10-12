# $Id: Makefile,v 1.11 2004/11/26 16:21:59 simonmar Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

PACKAGE = data
VERSION = 1.0
PACKAGE_DEPS = lang util

ALL_DIRS = edison edison/Assoc edison/Coll edison/Seq

SRC_HC_OPTS += -cpp -fglasgow-exts
SRC_HC_OPTS += -fallow-undecidable-instances -funbox-strict-fields

include $(TOP)/mk/target.mk
