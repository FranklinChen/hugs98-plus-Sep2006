#-----------------------------------------------------------------------------
# directories and files

all :: 

subdirs		= . Seq Coll Assoc

importdir	= Import
libdir		= Lib

VPATH		= $(subdirs) $(importdir) $(libdir)

fullsources     = $(foreach dir, $(subdirs), $(wildcard $(dir)/*.hs))
sources         = $(foreach file, $(fullsources), $(notdir $(file)))
objects         = $(foreach file, $(sources:.hs=.o), $(file))
dependclutter	= $(foreach file, $(fullsources:.hs=.hs.i), $(file))

#-----------------------------------------------------------------------------
# commands

#ghc		= /usr/fptools-BUILDS/new-rts-20/ghc/driver/ghc
ghc		= ghc
ghcflags	= -recomp -fglasgow-exts -fallow-undecidable-instances -i$(importdir) $(addprefix -i, $(subdirs)) -Wall -fno-warn-unused-matches -fno-warn-name-shadowing -funbox-strict-fields

#-----------------------------------------------------------------------------
# static pattern rules

$(objects) : %.o : %.hs
	$(ghc) -c $(ghcflags) $< -o $(libdir)/$(notdir $@)
	touch $(basename $<).hi
	cp $(basename $<).hi $(importdir)

#-----------------------------------------------------------------------------
# pattern rules

%.hi : %.o
	@if [ ! -f $@ ] ; then \
	echo $(RM) $< ; \
	$(RM) $< ; \
	set +e ; \
	echo $(MAKE) $(notdir $<) ; \
	$(MAKE) $(notdir $<) ; \
	if [ $$? -ne 0 ] ; then \
	exit 1; \
	fi ; \
	fi

#-----------------------------------------------------------------------------
# rules

.PHONY 	: echo depend

all :: libedison.a

libedison.a : $(objects)
	cd $(libdir); \
	ar rcv libedison.a $(objects); \
	ranlib libedison.a; \
	chmod a+r libedison.a

test :
	@echo $(sources)
	@echo $(objects)
	@echo $(dependclutter)
	@echo $(VPATH)

tar :
	cd ..; \
	rm -f edison.tar edison.tar.gz; \
	tar -cf edison.tar edison; \
	gzip edison.tar

depend:
	$(ghc) -M -optdep-f -optdepEdison.d0 $(ghcflags) $(fullsources)
	sed -e 's/[^ ]*\///' Edison.d0 | sed -e 's/[^ ]*\///' > Edison.d
	rm -f Edison.d0 Edison.d0.bak
	rm -f $(dependclutter)

clean :
	rm -f Import/*.hi Lib/*.o Lib/libedison.a

-include Edison.d
