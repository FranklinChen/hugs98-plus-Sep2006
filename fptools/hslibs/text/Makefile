# $Id: Makefile,v 1.23 2004/11/26 16:22:02 simonmar Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

PACKAGE = text
VERSION = 1.0
PACKAGE_DEPS = lang data

ALL_DIRS = html parsec

SRC_HC_OPTS += -cpp -fvia-C

parsec/ParsecToken_HC_OPTS += -fglasgow-exts
parsec/ParsecPerm_HC_OPTS += -fglasgow-exts

include $(TOP)/mk/target.mk
