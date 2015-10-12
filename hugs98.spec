# Requires %defines of `name', `version' and `release'.
# (`make rpm' takes care of these - you aren't expected to
# use this spec directly)

Name:         %{name}
Version:      %{version}
Release:      %{release}
License:      BSDish
Group:        Development/Languages/Haskell
URL:          http://haskell.org/hugs/
Source:       %{name}-%{version}.tar.gz
Packager:     Sven Panne <sven.panne@aedion.de>
BuildRoot:    %{_tmppath}/%{name}-buildroot
Provides:     haskell
Requires:     readline
Summary:      Hugs 98 - A Haskell Interpreter

%description
Hugs 98 is a functional programming system based on Haskell 98, the de facto
standard for non-strict functional programming languages. Hugs 98 provides an
almost complete implementation of Haskell 98, including:

* Lazy evaluation, higher order functions, and pattern matching.

* A wide range of built-in types, from characters to bignums, and lists to
  functions, with comprehensive facilities for defining new datatypes and type
  synonyms.

* An advanced polymorphic type system with type and constructor class
  overloading.

* All of the features of the Haskell 98 expression and pattern syntax including
  lambda, case, conditional and let expressions, list comprehensions,
  do-notation, operator sections, and wildcard, irrefutable and `as' patterns.

* An implementation of the Haskell 98 primitives for monadic I/O, with support
  for simple interactive programs, access to text files, handle-based I/O, and
  exception handling.

* An almost complete implementation of the Haskell module system. Hugs 98 also
  supports a number of advanced and experimental extensions including
  multi-parameter classes, extensible records, rank-2 polymorphism,
  existentials, scoped type variables, and restricted type synonyms.

%prep
%setup -q

%build
./configure --prefix=%{_prefix} --mandir=%{_mandir} ${EXTRA_CONFIGURE_OPTS}
make

%install
rm -rf ${RPM_BUILD_ROOT}
make DESTDIR=${RPM_BUILD_ROOT} install_all_but_docs
make -C docs DESTDIR=${RPM_BUILD_ROOT} install_man

%files
%defattr(-,root,root)
%doc Credits
%doc License
%doc Readme
%doc docs/ffi-notes.txt
%doc docs/libraries-notes.txt
%doc docs/machugs-notes.txt
%doc docs/server.html
%doc docs/server.tex
%doc docs/winhugs-notes.txt
%doc docs/users_guide/users_guide
%{_mandir}/man1/hugs.1.gz
%{_prefix}/bin/cpphs-hugs
%{_prefix}/bin/ffihugs
%{_prefix}/bin/hsc2hs-hugs
%{_prefix}/bin/hugs
%{_prefix}/bin/runhugs
%{_prefix}/lib/hugs/demos
%{_prefix}/lib/hugs/include
%{_prefix}/lib/hugs/oldlib
%{_prefix}/lib/hugs/packages
%{_prefix}/lib/hugs/programs
%{_prefix}/share/hsc2hs-0.67/template-hsc.h
