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
# <default>:   	  make hugs.exe and runhugs.exe
# install:	  make and install programs/libraries
# hugs.exe:	  make minimal working system
# runhugs.exe:	  make batch-mode version of Hugs
# hugsscript.dll: make DLL version of Hugs
# clean:       	  delete files not required in running system
# distclean:      delete files that can be regenerated using C compiler
# veryclean:      delete all machine generated files 
#                   (you need perl, bison/yacc, etc to rebuild these files)
# TAGS:           build emacs TAGS table

CC		= cl /nologo
CFLAGS		= -clr -DDOTNET -DMONTH_YEAR="\"Nov 2003\""
#CFLAGS		= -ML -Zi -DMONTH_YEAR="\"Nov 2003""
OPTFLAGS	= -O2

LD		= $(CC) -clr
LDFLAGS    	= 
# When debugging:
#LDFLAGS	= -Zi
DLL_FLAGS	= /LD
PIC_FLAGS	= 
LIBS		=  kernel32.lib advapi32.lib

YACC		= bison
RM		= -del
CP		= copy
EXEEXT		= .exe
OBJEXT		= obj

.SUFFIXES	:
.SUFFIXES	: .c .h .$(OBJEXT)

DOTNET_FILES    = dotnet\Invoker.cpp dotnet\HugsServ.cpp \
		  dotnet\HugsWrapper.cpp dotnet\prim.cpp
DOBJECTS        = dotnet\Invoker.$(OBJEXT) dotnet\HugsServ.$(OBJEXT) \
		  dotnet\prim.$(OBJEXT)  dotnet\HugsWrapper.$(OBJEXT)

HFILES		= HsFFI.h builtin.h char.h command.h config.h connect.h \
		  errors.h evaluator.h goal.h machdep.h module.h observe.h \
		  options.h opts.h output.h prelude.h script.h server.h \
		  storage.h strutil.h subst.h
CFILES		= hugs.c runhugs.c server.c edit.c observe.c \
		  builtin.c char.c compiler.c errors.c evaluator.c ffi.c \
		  goal.c input.c machdep.c machine.c module.c opts.c \
		  output.c plugin.c script.c static.c storage.c strutil.c \
		  subst.c type.c version.c $(DOTNET_FILES)
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
		  subst.$(OBJEXT) type.$(OBJEXT) version.$(OBJEXT)
IOBJECTS	= hugs.$(OBJEXT) edit.$(OBJEXT) observe.$(OBJEXT) $(OBJECTS) \
		  hugs.res $(DOBJECTS)

################################################################
# Default target
################################################################

# This rule goes first to make it the default choice
default		:: all

all		:: hugs.exe runhugs.exe ffihugs.exe hugsscript.dll

################################################################
# Hugs interpreter and standalone evaluator
################################################################

hugs$(EXEEXT)	: $(IOBJECTS)
		  $(LD) $(LDFLAGS) $(IOBJECTS)  $(LIBS) -o hugs$(EXEEXT)  $(LINK_FLAGS)
		  

SERVER_OBJECTS	= runhugs.res $(OBJECTS) server.$(OBJEXT) $(DOBJECTS)
runhugs.$(OBJEXT) : $(PRELUDE) hugs.c machdep.h timer.c runhugs.c
runhugs$(EXEEXT) : runhugs.$(OBJEXT) $(SERVER_OBJECTS)
		  $(LD) $(LDFLAGS) runhugs.$(OBJEXT) $(SERVER_OBJECTS) $(LIBS) -o runhugs$(EXEEXT) $(LINK_FLAGS)

ffihugs$(EXEEXT) : ffihugs.$(OBJEXT) $(SERVER_OBJECTS)
		  $(CC) $(LDFLAGS) ffihugs.$(OBJEXT) $(SERVER_OBJECTS) $(LIBS) -o ffihugs$(EXEEXT) $(LINK_FLAGS)

ffihugs.$(OBJEXT) : runhugs.$(OBJEXT)
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) -DFFI_COMPILER runhugs.c /Foffihugs.$(OBJEXT)

hugsscript.$(OBJEXT)  : hugsscript.c
hugsscript.dll  : $(SERVER_OBJECTS) hugsscript.$(OBJEXT)
		  $(CC) $(DLL_FLAGS) $(LDFLAGS) $(SERVER_OBJECTS) hugsscript.$(OBJEXT) $(LIBS) -o hugsscript.dll

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
	$(FFIHUGS) $(FFIHUGS_OPTS) +L../libraries/Network/initWinSock.c +Lwsock32.lib Network.Socket
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
		  $(RM) *.RES
		  $(RM) *.aux
		  $(RM) *.hp
distclean	:: clean
distclean	::
		  $(RM) hugs$(EXEEXT)
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

dotnet\prim.$(OBJEXT)	: dotnet\prim.cpp
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) -I. dotnet\prim.cpp /Fodotnet\prim.$(OBJEXT)

dotnet\Invoker.$(OBJEXT) : dotnet\Invoker.cpp
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) -I. dotnet\Invoker.cpp /Fodotnet\Invoker.$(OBJEXT)

dotnet\HugsServ.$(OBJEXT) : dotnet\HugsServ.cpp
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) -I. dotnet\HugsServ.cpp /Fodotnet\HugsServ.$(OBJEXT)

dotnet\HugsServ.cpp : dotnet\HugsServ.h

dotnet\HugsWrapper.$(OBJEXT) : dotnet\HugsWrapper.cpp
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) -I. dotnet\HugsWrapper.cpp /Fodotnet\HugsWrapper.$(OBJEXT)

dotnet\HugsWrapper.cpp : dotnet\HugsWrapper.h

dotnet\prim.$(OBJEXT) : $(PRELUDE) dotnet\prim.cpp dotnet\Invoker.h dotnet\Invoker.cpp

# Old & unused, but keep it around for now.
#dotnet\HugsWrapper.dll : hugs$(EXEEXT) dotnet\HugsWrapper.cs
#		  csc /t:library /unsafe /out:dotnet\HugsWrapper.dll dotnet\HugsWrapper.cs  /r:hugs$(EXEEXT)
#dotnet\HugsWrapper.dll: $(PRELUDE) dotnet\HugsWrapper.cs

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

parser.c	: parser.y
		  -$(YACC) parser.y
		  mv parser.tab.c parser.c

# veryclean	::
# 		  $(RM) parser.c

hugs.res	: msc\hugs.rc ..\icons\hugsicon.ico
		rc /imsc /i..\icons /r /fo hugs.res msc\hugs.rc 

runhugs.res	: msc\runhugs.rc ..\icons\hsxicon.ico
		rc /imsc /i..\icons /r /fo runhugs.res msc\runhugs.rc 

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

################################################################
# End of Makefile
################################################################
