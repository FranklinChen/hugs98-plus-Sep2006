# -----------------------------------------------------------------------------

TOP = ../..
include $(TOP)/mk/boilerplate.mk

H_FILES = $(filter-out HsOpenALConfig.h,$(wildcard *.h))

includedir = $(libdir)/include
INSTALL_INCLUDES = $(H_FILES)

DIST_CLEAN_FILES += HsOpenAL.h HsOpenALConfig.h

include $(TOP)/mk/target.mk
