# A (GNU) Makefile for building source and RPM distributions.

include Defs.mk

DARCS_GET	= darcs get --no-pristine-tree --partial

MONTH_YEAR = $(shell date +"%B %Y")
MON_YEAR = $(shell date +"%b%Y")
YEAR_MONTH_DAY = $(shell date +"%Y%m%d")

# convention: a release uses the MON_YEAR form of version,
# while a snapshot uses the YEAR_MONTH_DAY form.
# this should be sync'd with src/version.c
ifeq "$(MAJOR_RELEASE)" "1"
VERSION=$(MON_YEAR)
else
VERSION=$(YEAR_MONTH_DAY)
endif

PKGNAME = $(NAME)
PACKAGE = $(PKGNAME)-$(VERSION)

# Starting with Red Hat 8.0, the build functionality was removed from rpm, so
# one has to use rpmbuild instead. SuSE didn't follow this change, so there is
# no rpmbuild on SuSE and rpm still does the job. I like such changes...
RPMBUILD = rpmbuild

TMP    = /tmp
RPMTMP = $(TMP)/rpm
# probably should be uniqueified
TARTMP = $(TMP)/mktar

SPECFILE=$(NAME).spec

RPMDEFS = --define "_topdir $(RPMTMP)" \
	  --define "name $(NAME)" \
	  --define "version $(VERSION)" \
	  --define "release $(RELEASE)" \

VERSION_SUBSTS = \
    -e "s/define MAJOR_RELEASE.*/define MAJOR_RELEASE $(MAJOR_RELEASE)/" \
    -e "s/VERSION_STRING MONTH_YEAR/VERSION_STRING \"$(MONTH_YEAR)\"/" \
    -e "s/VERSION_STRING YYYYMMDD/VERSION_STRING \"$(YEAR_MONTH_DAY)\"/"

RC_STRING = RC1
RC_VERSION_SUBSTS = \
    -e "s/define MAJOR_RELEASE.*/define MAJOR_RELEASE $(MAJOR_RELEASE)/" \
    -e "s/VERSION_STRING MONTH_YEAR/VERSION_STRING \"$(MON_YEAR) $(RC_STRING)\"/"

tar: $(PACKAGE).tar.gz

# Utilities needed to pre-process fptools. To override
# these, set them on the command-line when invoking 'make':
#
#  foo$ make FIND=/usr/bin/find HAPPY=c:/happy/happy-1.15/bin/happy ...
#
FIND=find
HAPPY=happy

$(PACKAGE).tar.gz:
	-rm -rf $(TARTMP)
	-mkdir -p $(TARTMP)
# Note: The following line will not work correctly for "make -C blah".
	CVSROOT=$(CVS_ROOT); export CVSROOT; \
	  cd $(TARTMP); \
	  cvs export -r $(TAG) hugs98; \
	  cd hugs98; \
	  cvs export -r $(HSLIBSTAG) `for lib in $(HSLIBSDIRS); do echo fptools/hslibs/$$lib; done`
	cd $(TARTMP)/hugs98; mkdir packages
	cd $(TARTMP)/hugs98/packages; for lib in $(LIBRARIESDIRS); do $(DARCS_GET) $(DARCS_ROOT)/packages/$$lib; done
	cd $(TARTMP)/hugs98/packages; $(RM) -r */_darcs
	cd $(TARTMP)/hugs98/packages; $(RM) HaXml/configure
	cd $(TARTMP)/hugs98/packages; mv Cabal/Setup.lhs Cabal/examples/DefaultSetup.lhs
# preprocess, so the package can be built without happy
	if test -d $(TARTMP)/hugs98/packages/haskell-src; \
		then $(HAPPY) $(TARTMP)/hugs98/packages/haskell-src/Language/Haskell/Parser.ly; \
		else true; \
		fi
	cd $(TARTMP)/hugs98; $(DARCS_GET) $(DARCS_CPPHS)
	cd $(TARTMP)/hugs98; $(RM) -r cpphs/_darcs
	cd $(TARTMP)/hugs98; $(DARCS_GET) $(DARCS_ROOT)/hsc2hs
	cd $(TARTMP)/hugs98; $(RM) -r hsc2hs/_darcs
	cp $(TARTMP)/hugs98/src/version.c $(TARTMP)
	cd $(TARTMP)/hugs98/src; sed $(VERSION_SUBSTS) < $(TARTMP)/version.c > $(TARTMP)/hugs98/src/version.c
# using `make parser.c' would be best, but by default yacc
# will be used, and byacc is, for some reason, incompatible
	cd $(TARTMP)/hugs98/src; bison -y parser.y; mv y.tab.c parser.c
# Siggy deren't like these in distros
	if test "$(MAJOR_RELEASE)" -eq 1; then cd $(TARTMP)/hugs98; rm -rf tests; fi
	cd $(TARTMP)/hugs98; make configure
	cd $(TARTMP)/hugs98; $(RM) -r autom4te.cache libraries/autom4te.cache packages/*/autom4te.cache
	cd $(TARTMP)/hugs98; make debian/control
	mv $(TARTMP)/hugs98 $(TARTMP)/$(PACKAGE)
	cd $(TARTMP); tar cf $(TMP)/$(PKGNAME).tar $(PACKAGE)
	gzip -9 $(TMP)/$(PKGNAME).tar
	mv $(TMP)/$(PKGNAME).tar.gz $(PACKAGE).tar.gz

rpm-dirs:
	-mkdir $(RPMTMP)
	-mkdir $(RPMTMP)/BUILD
	-mkdir $(RPMTMP)/RPMS
	-mkdir $(RPMTMP)/SOURCES
	-mkdir $(RPMTMP)/SPECS
	-mkdir $(RPMTMP)/SRPMS

rpm: tar rpm-dirs
	cp $(PACKAGE).tar.gz $(RPMTMP)/SOURCES
	$(RPMBUILD) $(RPMDEFS) -ba $(SPECFILE)
	mv $(RPMTMP)/RPMS/i?86/$(PACKAGE)-$(RELEASE).i?86.rpm .
	mv $(RPMTMP)/SRPMS/$(PACKAGE)-$(RELEASE).src.rpm .

rc-rpm:
	$(MAKE) VERSION_SUBSTS='$(RC_VERSION_SUBSTS)' rpm
