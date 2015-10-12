# Generated automatically from Makefile.in by configure.
# --------------------------------------------------------------------------
# Makefile for Hugs
#
# The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
# Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
# 1994-2003, All rights reserved.  It is distributed as
# free software under the license in the file "License", which is
# included in the distribution.
# --------------------------------------------------------------------------

# Targets:
#
# <default>:   	 make hugs.exe and runhugs.exe
# install:	 make and install programs/libraries
# hugs.exe:    	 make minimal working system
# runhugs.exe:	 make batch-mode version of Hugs
# clean:       	 delete files not required in running system
# distclean:     delete files that can be regenerated using C compiler
# veryclean:     delete all machine generated files 
#                  (you need perl, bison/yacc, etc to rebuild these files)
# check:	 run regression tests
# TAGS:          build emacs TAGS table

# This rule goes first to make it the default choice
default		:: hugs.exe runhugs.exe

CC		= bcc32
CFLAGS		= -j5 -w-pia -w-aus -w-par -w-rvl* 
OPTFLAGS	= -d -y-

LD		= ld
LDFLAGS    	=   
DLL_FLAGS	= -WDE
PIC_FLAGS	= 
LIBS		=  

YACC		= bison
RM		= DEL
CP		= COPY



.SUFFIXES	:
.SUFFIXES	: .c .h .obj
HFILES		= prelude.h config.h options.h storage.h connect.h \
		  errors.h command.h server.h HsFFI.h module.h machdep.h script.h
CFILES		= hugs.c storage.c input.c static.c type.c subst.c \
		  output.c compiler.c machine.c interp.c builtin.c \
		  server.c ffi.c module.c machdep.c script.c
INCFILES	= parser.c preds.c bignums.c scc.c timer.c \
		  printer.c iomonad.c interns.c array.c
YFILES		= parser.y
SOURCES		= $(HFILES) $(CFILES) $(INCFILES) $(YFILES) 
OBJECTS		= storage.obj input.obj static.obj type.obj compiler.obj \
		  subst.obj plugin.obj builtin.obj machine.obj output.obj \
		  ffi.obj version.obj module.obj machdep.obj
IOBJECTS	= hugs.obj $(OBJECTS)
PRELUDE		= config.h options.h prelude.h script.h

################################################################
# Hugs interpreter and standalone evaluator
################################################################

hugs.exe	: $(IOBJECTS)  
		  $(CC) $(LDFLAGS) @iobjects.rsp $(LIBS) -ohugs.exe 
		  

SERVER_OBJECTS	= server.obj $(OBJECTS) 
runhugs.obj	: $(PRELUDE) hugs.c machdep.h timer.c runhugs.c
runhugs.exe	: runhugs.obj $(SERVER_OBJECTS)
		  $(CC) $(LDFLAGS) runhugs.obj @sobjects.rsp $(LIBS) -orunhugs.exe

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
		  $(RM) hugs.exe
		  $(RM) runhugs.exe
		  $(RM) *.pdf
		  $(RM) TAGS
		  $(RM) *~
veryclean	:: distclean
veryclean	::
		  $(RM) options.h
		  $(RM) config.h
		  $(RM) *.rsp

TAGS		::
		  etags *.[ych]

################################################################
# Dependencies
################################################################

.c.obj	:
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) $<

# These are compiled with less optimisation to avoid optimisation bugs in
# certain compilers.  This may be overly conservative on some compilers.
compiler.obj	: compiler.c
		  $(CC) -c $(CFLAGS) compiler.c
static.obj	: static.c
		  $(CC) -c $(CFLAGS) static.c

# parser.c	: parser.y
# 		  -$(YACC) parser.y
# 		  mv y.tab.c parser.c
# veryclean	::
# 		  $(RM) parser.c

server.obj	: $(PRELUDE) storage.h connect.h errors.h \
		  command.h machdep.h timer.c hugs.c server.c server.h
hugs.obj	: $(PRELUDE) storage.h connect.h errors.h \
		  command.h machdep.h timer.c
storage.obj	: $(PRELUDE) storage.h connect.h errors.h
input.obj	: $(PRELUDE) storage.h connect.h errors.h \
		  parser.c command.h module.h
subst.obj	: $(PRELUDE) storage.h connect.h errors.h subst.h
static.obj	: $(PRELUDE) storage.h connect.h errors.h subst.h \
		  scc.c module.h
type.obj	: $(PRELUDE) storage.h connect.h errors.h subst.h \
		  scc.c preds.c
output.obj	: $(PRELUDE) storage.h connect.h errors.h
compiler.obj	: $(PRELUDE) storage.h connect.h errors.h 
machine.obj	: $(PRELUDE) storage.h connect.h errors.h
plugin.obj	: $(PRELUDE) storage.h connect.h errors.h
builtin.obj	: $(PRELUDE) storage.h connect.h errors.h \
		  bignums.c printer.c iomonad.c interns.c array.c
ffi.obj		: $(PRELUDE) storage.h connect.h errors.h
module.obj	: $(PRELUDE) storage.h connect.h errors.h module.h
machdep.obj	: $(PRELUDE) storage.h connect.h errors.h machdep.h
script.obj	: $(PRELUDE) storage.h HsFFI.h connect.h errors.h 

################################################################
# Regression tests (none supplied)
################################################################

check		:


################################################################
# End of Makefile
################################################################
