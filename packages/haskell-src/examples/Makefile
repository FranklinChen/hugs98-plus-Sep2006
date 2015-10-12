# -----------------------------------------------------------------------------

TOP = ../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

ifeq "$(way)" ""
HS_PROG        = hsparser$(exeext)
endif

HC             = $(GHC_INPLACE)
MKDEPENDHS     = $(GHC_INPLACE)
SRC_HC_OPTS   += -package haskell-src

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
