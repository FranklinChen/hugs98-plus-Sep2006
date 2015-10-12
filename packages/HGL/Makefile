# -----------------------------------------------------------------------------

TOP = ..
include $(TOP)/mk/boilerplate.mk
-include config.mk

ifneq "$(findstring clean, $(MAKECMDGOALS))" ""
# if we're cleaning, then config.mk might have been cleaned already
HGL_BUILD_PACKAGE=yes
PACKAGE=HGL
endif

# -----------------------------------------------------------------------------

ifeq "$(HGL_BUILD_PACKAGE)" "yes"

# We do not make examples a subdir because then we can't avoid building it
# SUBDIRS = examples
SUBDIRS = 

ALL_DIRS = \
	Graphics \
	Graphics/HGL \
	Graphics/HGL/Draw \
	Graphics/HGL/Internals

ifneq "$(PLATFORM)" ""
ALL_DIRS      += Graphics/HGL/$(PLATFORM)
endif

PACKAGE_DEPS   = base $(PLATFORM)

SRC_HC_OPTS   += -cpp -Iinclude

PACKAGE_CPP_OPTS += -DMAINTAINER=$(MAINTAINER)

SRC_HADDOCK_OPTS += -t "Graphics Libraries ($(PACKAGE) package)"

# -----------------------------------------------------------------------------

HGL/Graphics/HGL/Draw/Brush.$(way_)o \
HGL/Graphics/HGL/Draw/Font.$(way_)o \
HGL/Graphics/HGL/Draw/Pen.$(way_)o \
HGL/Graphics/HGL/Draw/Picture.$(way_)o \
HGL/Graphics/HGL/Draw/Region.$(way_)o \
HGL/Graphics/HGL/Draw/Text.$(way_)o \
HGL/Graphics/HGL/Internals/Draw.$(way_)o \
HGL/Graphics/HGL/Internals/Types.$(way_)o \
HGL/Graphics/HGL/Key.$(way_)o \
HGL/Graphics/HGL/Run.$(way_)o \
HGL/Graphics/HGL/Window.$(way_)o \ : include/HsHGLConfig.h

endif

EXCLUDED_SRCS += Setup.hs

# -----------------------------------------------------------------------------

DIST_CLEAN_FILES += include/HsHGLConfig.h HGL.buildinfo config.cache config.status
DIST_CLEAN_FILES += config.mk

extraclean::
	$(RM) -rf autom4te.cache

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
