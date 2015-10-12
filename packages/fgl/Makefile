# -----------------------------------------------------------------------------

TOP=..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

ALL_DIRS = \
	Data/Graph \
	Data/Graph/Inductive \
	Data/Graph/Inductive/Internal \
	Data/Graph/Inductive/Monad \
	Data/Graph/Inductive/Query

PACKAGE = fgl
VERSION = 5.2
PACKAGE_DEPS = base mtl

SRC_HC_OPTS += -Wall -fno-warn-missing-signatures -fglasgow-exts -fallow-overlapping-instances

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)"

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
