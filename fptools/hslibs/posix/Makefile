# $Id: Makefile,v 1.34 2005/09/20 16:29:09 ross Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

PACKAGE = posix
VERSION = 1.0
PACKAGE_DEPS = lang unix

SUBDIRS = cbits

SRC_HC_OPTS += -cpp -fglasgow-exts -fvia-C -Icbits

SRC_CC_OPTS += -I../../ghc/includes

include $(TOP)/mk/target.mk
