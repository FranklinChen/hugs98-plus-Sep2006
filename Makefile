# Top-level Makefile for Hugs
# (this should be a POSIX 1003.2-1992 Makefile)

# Start of general settings (leave this line unchanged)

NAME = hugs98

# Set to 0 if a snapshot release.
MAJOR_RELEASE = 1

# Release number of RPM.
RELEASE = 1

TAG = HEAD
HSLIBSTAG = HEAD

CVS_ROOT = :pserver:anoncvs@cvs.haskell.org:/cvs
HSLIBSDIRS = concurrent data hssource lang net text util posix

DARCS_ROOT = http://darcs.haskell.org
LIBRARIESDIRS = base haskell98 haskell-src mtl network parsec QuickCheck unix \
	Cabal OpenGL GLUT OpenAL ALUT fgl X11 HGL HaXml HUnit Win32 time stm \
	xhtml
DARCS_CPPHS = http://www.cs.york.ac.uk/fp/darcs/cpphs

# End of general settings (leave this line unchanged)

PACKAGES	= packages/base/base.cabal
DARCS_GET	= darcs get --partial

# General targets:
#
# all:		(default) build a system that can be run in-place.
# install:	all + install in $(prefix) as supplied to configure.
# clean:	delete files generated in building, except those needed
#		for in-place use.
# distclean:	delete files created by configuring or building.
# veryclean:	delete most things that can be regenerated (though this
#		will require additional tools).
# check:	run regression tests

# Note: we have to build in src before building in libraries, as the
# latter uses the binaries in src.  See libraries/Makefile.in for
# details of the bootstrapping of the libraries.

all: $(PACKAGES) src/Makefile
	cd src; $(MAKE) all
	cd libraries; $(MAKE) all
	cd docs; $(MAKE) all

# We install the standard libraries and the simple demos.
# We don't install things which don't work on Unix (e.g. Win32).

install: install_all_but_docs
	cd docs; $(MAKE) install

# Install everything except documentation, which is installed differently
# by some packagers (e.g. rpm)

install_all_but_docs: $(PACKAGES) src/Makefile
	cd src; $(MAKE) install
	cd libraries; $(MAKE) install
	cd demos; $(MAKE) install

clean: clean_root
	cd src; if test -f Makefile; then $(MAKE) clean; fi
	cd libraries; if test -f Makefile; then $(MAKE) clean; fi
	cd docs; if test -f Makefile; then $(MAKE) clean; fi
	cd demos; if test -f Makefile; then $(MAKE) clean; fi

distclean: distclean_root
	cd src; if test -f Makefile; then $(MAKE) distclean; fi
	cd libraries; if test -f Makefile; then $(MAKE) distclean; fi
	cd docs; if test -f Makefile; then $(MAKE) distclean; fi
	cd demos; if test -f Makefile; then $(MAKE) distclean; fi

veryclean: veryclean_root
	cd src; if test -f Makefile; then $(MAKE) veryclean; fi
	cd libraries; if test -f Makefile; then $(MAKE) veryclean; fi
	cd docs; if test -f Makefile; then $(MAKE) veryclean; fi
	cd demos; if test -f Makefile; then $(MAKE) veryclean; fi

clean_root:
	$(RM) *.tar.gz *.rpm Defs.mk
	$(RM) *~

distclean_root: clean_root
	$(RM) -r config.status config.log config.cache autom4te.cache
	$(RM) MkDefs tests/config

veryclean_root: distclean_root
	cd debian; $(RM) control hugs.copyright libhugs-*-bundled.*

################################################################
# Regression tests (Unix only)
#
# Uses runstdtest (from ghc-0.26/ghc/glafp-utils/scripts), perl 5
# and /bin/sh (Bourne shell).
#
# "make verbosecheck" generates a lot of output to explain what is going on
# and reassure you that progress is being made.  This is great if you've
# never run these tests before - but if you just want to reassure yourself
# that nothing has broken since the last release, you might prefer to
# run "make check" which removes all the explanations and success
# stories - leaving just the errors (if any).
#
################################################################

check: all
	cd tests && sh testScript | if egrep -v '^--( |-----)'; then false; else true; fi

verbosecheck: all
	cd tests && sh testScript

# Building distributions

tarplus: Defs.mk
	$(MAKE) -f RPM.mk tar

tar: Defs.mk
	$(MAKE) PKGNAME=$(NAME) LIBRARIESDIRS='base haskell98 Cabal' -f RPM.mk tar

rpm: Defs.mk
	$(MAKE) -f RPM.mk rpm

Defs.mk: Makefile
	( echo '# Automatically extracted from Makefile (so edit that instead)';\
	  sed -n '/^# Start of general settings/,/^# End of general settings/p' Makefile;\
	) >$@

# Build phases:

# configuration

src/Makefile: configure
	$(RM) -r config.cache autom4te.cache
	LIBS=$(GNULIBS) ./configure $(EXTRA_CONFIGURE_OPTS)

configure: configure.ac aclocal.m4 $(PACKAGES)
	for dir in packages/*; do if test -f $$dir/configure.ac; \
		then (cd $$dir; autoreconf); fi; done
	-autoreconf

# fetching library sources and utility programs

$(PACKAGES):
	cvs -d $(CVS_ROOT) checkout -r $(HSLIBSTAG) `for lib in $(HSLIBSDIRS); do echo fptools/hslibs/$$lib; done`
	$(RM) -r packages cpphs hsc2hs
	mkdir packages
	for lib in $(LIBRARIESDIRS); do $(DARCS_GET) --repo-name=packages/$$lib $(DARCS_ROOT)/packages/$$lib; done
# We don't use this, so don't leave it there for Cabal to run
	cd packages; $(RM) HaXml/configure
# Move these so that make_bootlib won't convert them
	cd packages; mv Cabal/*.lhs Cabal/examples
	$(DARCS_GET) $(DARCS_CPPHS)
	$(DARCS_GET) $(DARCS_ROOT)/hsc2hs

debian/control: debian/control.in debian/make-control.hs
	cp License debian/hugs.copyright
# We need runhugs to build a Debian source package from CVS,
# but don't complain if it's unavailable.
	-runhugs -98 debian/make-control.hs `ls packages/*/*.cabal | grep -v Win32` 2>/dev/null
