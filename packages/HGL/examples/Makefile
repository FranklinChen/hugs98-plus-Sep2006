# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.6 2004/05/05 11:39:13 ross Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

#
# Disable 'make boot'
#
NO_BOOT_TARGET=YES

WAYS=

# -----------------------------------------------------------------------------

EXAMPLES    := $(wildcard *.hs)
BINS        := $(EXAMPLES:.hs=$(exeext))
CLEAN_FILES += $(BINS)

HC           = $(GHC_INPLACE)
MKDEPENDHS   = $(GHC_INPLACE)
SRC_HC_OPTS += -package HGL

all:: $(BINS)

$(BINS): %$(exeext): %.hs
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $<

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
