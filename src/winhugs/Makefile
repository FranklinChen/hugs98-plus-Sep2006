# Generated automatically from Makefile.in by configure.
# --------------------------------------------------------------------------
# Makefile for Hugs
#
# The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
# Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
# 1994-2004, All rights reserved.  It is distributed as
# free software under the license in the file "License", which is
# included in the distribution.
# --------------------------------------------------------------------------

# Targets:
#
# <default>:   	  make winhugs.exe
# install:	  make and install programs/libraries
# winhugs.exe: 	  make minimal working system
# runhugs.exe:	  make batch-mode version of Hugs
# clean:       	  delete files not required in running system
# distclean:      delete files that can be regenerated using C compiler
# veryclean:      delete all machine generated files 
#                   (you need perl, bison/yacc, etc to rebuild these files)
# TAGS:           build emacs TAGS table

CC		= cl /nologo
VERSTR          = -DMONTH_YEAR="\"Nov 2003\""
CFLAGS		= -MD -I. $(VERSTR)
# When debugging:
#CFLAGS		= -Zi -MDd -I. $(VERSTR)
OPTFLAGS	= -O2
RC		= rc

LD		= ld
LDFLAGS    	= 
DLL_FLAGS	= /LD
PIC_FLAGS	= 
LIBS		= kernel32.lib user32.lib gdi32.lib comdlg32.lib shell32.lib advapi32.lib

YACC		= bison
RM		= -del
CP		= copy
EXEEXT		= .exe
OBJEXT		= obj

.SUFFIXES	:
.SUFFIXES	: .c .h .$(OBJEXT)

WOBJECTS        = winhugs.$(OBJEXT) winframe.$(OBJEXT) wintext.$(OBJEXT) \
		  winhint.$(OBJEXT) winstln.$(OBJEXT) wintoolb.$(OBJEXT) \
		  winutils.$(OBJEXT) 

HFILES		= HsFFI.h builtin.h char.h command.h config.h connect.h \
		  errors.h evaluator.h goal.h machdep.h module.h observe.h \
		  options.h opts.h output.h prelude.h script.h server.h \
		  storage.h strutil.h subst.h
CFILES		= hugs.c runhugs.c server.c edit.c observe.c \
		  builtin.c char.c compiler.c errors.c evaluator.c ffi.c \
		  goal.c input.c machdep.c machine.c module.c opts.c \
		  output.c plugin.c script.c static.c storage.c strutil.c \
		  subst.c type.c version.c \
		  Winhugs.c
INCFILES	= array.c bignums.c dirprim.c interns.c iomonad.c \
		  preds.c printer.c scc.c timeprim.c timer.c
YFILES		= parser.y
SOURCES		= $(HFILES) $(CFILES) $(INCFILES) $(YFILES) 
OBJECTS		= builtin.$(OBJEXT) char.$(OBJEXT) compiler.$(OBJEXT) \
		  errors.$(OBJEXT) evaluator.$(OBJEXT) ffi.$(OBJEXT) \
		  goal.$(OBJEXT) input.$(OBJEXT) machdep.$(OBJEXT) \
		  machine.$(OBJEXT) module.$(OBJEXT) opts.$(OBJEXT) \
		  output.$(OBJEXT) plugin.$(OBJEXT) script.$(OBJEXT) \
		  static.$(OBJEXT) storage.$(OBJEXT) strutil.$(OBJEXT) \
		  subst.$(OBJEXT) type.$(OBJEXT) version.$(OBJEXT) \
		  $(WOBJECTS)
IOBJECTS	= hugs.$(OBJEXT) edit.$(OBJEXT) observe.$(OBJEXT) $(OBJECTS)

################################################################
# Default target
################################################################

# This rule goes first to make it the default choice
default		:: all

all		:: winhugs$(EXEEXT)

################################################################
# Hugs interpreter and standalone evaluator
################################################################

winhugs$(EXEEXT) : $(IOBJECTS)  winhugs.res
		  $(CC) $(LDFLAGS) $(IOBJECTS) winhugs.res  $(LIBS) -o winhugs$(EXEEXT) -link /subsystem:windows 


SERVER_OBJECTS	= server.$(OBJEXT) $(OBJECTS) 
runhugs$(EXEEXT) : runhugs.$(OBJEXT) $(SERVER_OBJECTS)
		  $(CC) $(LDFLAGS) runhugs.$(OBJEXT) $(SERVER_OBJECTS) $(LIBS) -o runhugs$(EXEEXT)

#
# Create all the FFI extension DLLs.
#
FFIHUGS=..\ffihugs
FFIHUGS_OPTS=+G -98 -P

.PHONY: ffi-dlls

ffi-dlls:
	$(FFIHUGS) $(FFIHUGS_OPTS) +L../libraries/Hugs/Storable_aux.c Hugs.Storable
	$(FFIHUGS) $(FFIHUGS_OPTS) Foreign.Marshal.Alloc
	$(FFIHUGS) $(FFIHUGS_OPTS) Foreign.Marshal.Utils
	$(FFIHUGS) $(FFIHUGS_OPTS) +L../libraries/Foreign/C/errno.c Foreign.C.Error
	$(FFIHUGS) $(FFIHUGS_OPTS) +L../libraries/Network/initWinSock.c +L../libraries/Network/winSockErr.c +Lwsock32.lib Network.Socket
	$(FFIHUGS) $(FFIHUGS_OPTS) +Lwsock32.lib Network.BSD

################################################################
# Clean, distclean, veryclean, TAGS
################################################################

clean		::
		  $(RM) *.o
		  $(RM) *.O
		  $(RM) *.obj
		  $(RM) *.OBJ
		  $(RM) *.LIB
		  $(RM) *.DEF
		  $(RM) *.RES
		  $(RM) *.EXP
		  $(RM) *.ILK
		  $(RM) *.PDB
		  $(RM) *.TD2
		  $(RM) *.MAP
		  $(RM) *.CSM
		  $(RM) *.TR2
		  $(RM) *.DSW
		  $(RM) *.aux
		  $(RM) *.hp
distclean	:: clean
distclean	::
		  $(RM) winhugs$(EXEEXT)
		  $(RM) runhugs$(EXEEXT)
		  $(RM) ffihugs$(EXEEXT)
		  $(RM) *.pdf
		  $(RM) TAGS
		  $(RM) *~
veryclean	:: distclean
veryclean	::
		  $(RM) config.h
		  $(RM) options.h
TAGS		::
		  etags *.[ych]

################################################################
# C and Yacc rules
################################################################

.c.$(OBJEXT)	:
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) $<

# Modules to be compiled without optimization.
# (old comment: to avoid optimisation bugs in certain compilers.
#               This may be overly conservative on some compilers.)
# (The following explanation is based on a posting by Alastair Reid.)
# These modules allocate cells on the Hugs heap and assume a conservative
# garbage collector.  On some (especially RISC) architectures, the
# optimizer may identify a pointer to a Cell as a common subexpression,
# and hold that instead of the Cell.  This would then be missed by the
# conservative garbage collector's simplistic scan of the C stack.
# Modules associated with evaluation are safe because they don't assume
# conservative GC (see IMPORTANT NOTICE in builtin.c).
compiler.$(OBJEXT) : compiler.c
		  $(CC) -c $(CFLAGS) compiler.c
static.$(OBJEXT) : static.c
		  $(CC) -c $(CFLAGS) static.c

# parser.c	: parser.y
# 		  -$(YACC) parser.y
# 		  mv y.tab.c parser.c
# veryclean	::
# 		  $(RM) parser.c

#dependencies for these files are incomplete

wintext.$(OBJEXT)	: $(HFILES) "winhugs\wintext.c"
		  $(CC) -c $(CFLAGS) $(OPTFLAGS)  "winhugs\wintext.c" 

winframe.$(OBJEXT)    : $(HFILES) "winhugs\winframe.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winframe.c" 

winhint.$(OBJEXT)	: $(HFILES) "winhugs\winhint.c"
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winhint.c" 

winstln.$(OBJEXT)	: $(HFILES) "winhugs\winstln.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winstln.c" 

wintoolb.$(OBJEXT)    : $(HFILES) "winhugs\wintoolb.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\wintoolb.c" 

winutils.$(OBJEXT)    : $(HFILES) "winhugs\winutils.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winutils.c" 

winhugs.$(OBJEXT)    : $(HFILES) "winhugs\winhugs.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winhugs.c" 

winhugs.res     : "winhugs\winhugs.rc"
                  $(RC) -fowinhugs.res "winhugs\winhugs.rc"

# WinHugs modules
#
# The dependencies could be made tighter by not using HFILES throughout,
# but individually naming the header files needed by each file. Elect
# not to do this due to the overhead of having to keep it up-to-date.
winhugs.$(OBJEXT): $(HFILES) \
  winhugs/winhugs.c winhugs/menusbm.c winhugs/WinBrows.c winhugs/WinHugs.h \
  winhugs/WinFrame.h winhugs/WinToolB.h winhugs/WinSTLN.h winhugs/WinUtils.h
winframe.$(OBJEXT): $(HFILES) winhugs/winframe.c \
  winhugs/WinFrame.h winhugs/WinSTLN.h winhugs/WinToolB.h winhugs/WinUtils.h
winhint.$(OBJEXT): $(HFILES) winhugs/winhint.c \
  winhugs/WinHint.h winhugs/WinUtils.h
winstln.$(OBJEXT): $(HFILES) winhugs/winstln.c \
  winhugs/WinSTLN.h winhugs/WinUtils.h
wintoolb.$(OBJEXT): $(HFILES) winhugs/wintoolb.c \
  winhugs/WinToolB.h winhugs/WinHint.h winhugs/WinUtils.h
winutils.$(OBJEXT): $(HFILES) winhugs/winutils.c winhugs/WinUtils.h

################################################################
# Generated object dependencies (Don't change or delete this line)
################################################################

hugs.$(OBJEXT): hugs.c prelude.h config.h platform.h options.h storage.h HsFFI.h \
  command.h connect.h errors.h script.h opts.h strutil.h evaluator.h \
  machdep.h output.h module.h timer.c
runhugs.$(OBJEXT): runhugs.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h machdep.h observe.h builtin.h evaluator.h errors.h \
  server.h HugsAPI.h
server.$(OBJEXT): server.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h script.h machdep.h evaluator.h opts.h strutil.h \
  errors.h server.h HugsAPI.h
edit.$(OBJEXT): edit.c prelude.h config.h platform.h options.h storage.h HsFFI.h \
  connect.h errors.h opts.h strutil.h machdep.h
observe.$(OBJEXT): observe.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h command.h errors.h machdep.h builtin.h output.h \
  observe.h
builtin.$(OBJEXT): builtin.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h machdep.h char.h builtin.h bignums.c \
  printer.c iomonad.c timeprim.c dirprim.c interns.c array.c
char.$(OBJEXT): char.c prelude.h config.h platform.h options.h storage.h HsFFI.h \
  connect.h errors.h char.h unitable.c
compiler.$(OBJEXT): compiler.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h goal.h char.h output.h opts.h
errors.$(OBJEXT): errors.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h output.h opts.h goal.h char.h evaluator.h
evaluator.$(OBJEXT): evaluator.c prelude.h config.h platform.h options.h \
  storage.h HsFFI.h connect.h errors.h script.h output.h strutil.h opts.h \
  machdep.h evaluator.h
ffi.$(OBJEXT): ffi.c prelude.h config.h platform.h options.h storage.h HsFFI.h \
  connect.h errors.h output.h strutil.h
goal.$(OBJEXT): goal.c prelude.h config.h platform.h options.h storage.h HsFFI.h \
  connect.h errors.h machdep.h opts.h goal.h
input.$(OBJEXT): input.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h command.h errors.h module.h script.h opts.h goal.h \
  machdep.h char.h parser.c
machdep.$(OBJEXT): machdep.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h opts.h strutil.h machdep.h char.h \
  evaluator.h
machine.$(OBJEXT): machine.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h char.h opts.h
module.$(OBJEXT): module.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h module.h output.h
opts.$(OBJEXT): opts.c prelude.h config.h platform.h options.h storage.h HsFFI.h \
  connect.h errors.h machdep.h strutil.h opts.h char.h
output.$(OBJEXT): output.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h output.h char.h
plugin.$(OBJEXT): plugin.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h
script.$(OBJEXT): script.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h machdep.h opts.h strutil.h script.h
static.$(OBJEXT): static.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h machdep.h errors.h output.h subst.h module.h opts.h \
  goal.h scc.c
storage.$(OBJEXT): storage.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h opts.h errors.h machdep.h evaluator.h strutil.h \
  output.h
strutil.$(OBJEXT): strutil.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h strutil.h
subst.$(OBJEXT): subst.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h output.h opts.h subst.h
type.$(OBJEXT): type.c prelude.h config.h platform.h options.h storage.h HsFFI.h \
  connect.h errors.h output.h subst.h goal.h opts.h preds.c scc.c
version.$(OBJEXT): version.c prelude.h config.h platform.h options.h

################################################################
# End of generated object dependencies (Don't change or delete this line)
################################################################

version.$(OBJEXT): echodate.h

echodate.h: echodate$(EXEEXT)
	$(PWD)/echodate$(EXEEXT) > echodate.h

echodate$(EXEEXT): msc/echodate.c
	$(CC) $(LDFLAGS) msc/echodate.c -o $@

################################################################
# End of Makefile
################################################################
