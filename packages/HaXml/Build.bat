REM -- Build HaXml package using GHC
REM
REM    Usage: (case-sensitive)
REM      Build         compile and install the HaXml library GHC package
REM      Build Remove  remove the HaXml GHC library package
REM      Build Tools   compile the tools shipped with HaXml

rem -- Change the following variables (upto and including SRC)
rem -- to suit the local system environment --

rem    GHC version
set GHCVER=6.4.2

rem    GHC installation directory:
set GHCDIR=C:\DEV\ghc\ghc-%GHCVER%

rem    Programs needed to build HaXml:
rem
rem    NOTE: install MinGW linked from <http://www.mingw.org/>
rem    for a copy of 'ar.exe'
rem
set GHC=C:\DEV\ghc\ghc-%GHCVER%\bin\ghc.exe
set GHCPKG=C:\DEV\ghc\ghc-%GHCVER%\bin\ghc-pkg.exe
set AR=C:\DEV\MinGW\bin\ar.exe
set LD=C:\DEV\ghc\ghc-%GHCVER%\gcc-lib\ld.exe

rem    Source directory for HaXml:
set SRC=C:\DEV\Haskell\lib\HaXml-1.17\src

rem    Two very long lines (500-600 chars) follow here.  
rem    They should not need changing.
set SRCS=Text/XML/HaXml.hs Text/XML/HaXml/Combinators.hs Text/XML/HaXml/Posn.hs Text/XML/HaXml/Lex.hs  Text/XML/HaXml/Parse.hs Text/XML/HaXml/Pretty.hs Text/XML/HaXml/Types.hs Text/XML/HaXml/Validate.hs Text/XML/HaXml/Wrappers.hs Text/XML/HaXml/OneOfN.hs Text/XML/HaXml/XmlContent.hs Text/XML/HaXml/TypeMapping.hs Text/XML/HaXml/Verbatim.hs Text/XML/HaXml/Escape.hs Text/XML/HaXml/SAX.hs Text/XML/HaXml/Html/Generate.hs Text/XML/HaXml/Html/Parse.hs Text/XML/HaXml/Html/Pretty.hs Text/XML/HaXml/Xtract/Combinators.hs Text/XML/HaXml/Xtract/Lex.hs Text/XML/HaXml/Xtract/Parse.hs Text/ParserCombinators/Poly.hs Text/ParserCombinators/PolyState.hs Text/ParserCombinators/TextParser.hs Text/ParserCombinators/PolyLazy.hs Text/ParserCombinators/PolyStateLazy.hs Text/XML/HaXml/ParseLazy.hs Text/XML/HaXml/Html/ParseLazy.hs
set OBJS=Text/XML/HaXml.o Text/XML/HaXml/Combinators.o Text/XML/HaXml/Posn.o Text/XML/HaXml/Lex.o  Text/XML/HaXml/Parse.o Text/XML/HaXml/Pretty.o Text/XML/HaXml/Types.o Text/XML/HaXml/Validate.o Text/XML/HaXml/Wrappers.o Text/XML/HaXml/OneOfN.o Text/XML/HaXml/XmlContent.o Text/XML/HaXml/TypeMapping.o Text/XML/HaXml/Verbatim.o Text/XML/HaXml/Escape.o Text/XML/HaXml/SAX.o Text/XML/HaXml/Html/Generate.o Text/XML/HaXml/Html/Parse.o Text/XML/HaXml/Html/Pretty.o Text/XML/HaXml/Xtract/Combinators.o Text/XML/HaXml/Xtract/Lex.o Text/XML/HaXml/Xtract/Parse.o Text/ParserCombinators/Poly.o Text/ParserCombinators/PolyState.o Text/ParserCombinators/TextParser.o Text/ParserCombinators/PolyLazy.o Text/ParserCombinators/PolyStateLazy.o Text/XML/HaXml/ParseLazy.o Text/XML/HaXml/Html/ParseLazy.o


rem -- Get on with the real work --

if "%1"=="Remove" goto Remove
if "%1"=="Tools" goto Tools

rem -- Compile sources and create library archive
if "%GHCVER%"=="6.4" COPY HaXml.cabal %SRC%\pkg.conf
cd %SRC%
%GHC% --make -cpp -i. -package-name HaXml %SRCS%
%AR% r libHSHaXml.a %OBJS%

rem -- Create library file for GHCi
%LD% -r --whole-archive -o HSHaXml.o libHSHaXml.a

rem -- Install the library archive(s) where GHC can find them
COPY libHSHaXml.a %GHCDIR%
COPY HSHaXml.o    %GHCDIR%

rem -- Install the interface files where GHC can find them
rem    /L - list only, /Y - overrite without confirmation
rem    /S - copy subdirectories, /T - create directories only
rem    /F - display full filenames while copying
XCOPY /S /F *.hi %GHCDIR%\imports

rem -- Finally, register the package with GHC
if "%GHCVER%"=="6.2.2" %GHCPKG% --add-package -i pkg.conf
if "%GHCVER%"=="6.4.2"   ECHO import-dirs:   %GHCDIR%\imports >>pkg.conf
if "%GHCVER%"=="6.4.2"   ECHO library-dirs:  %GHCDIR% >>pkg.conf
if "%GHCVER%"=="6.4.2"   ECHO depends:       base, haskell98 >>pkg.conf
if "%GHCVER%"=="6.4.2"   ECHO hs-libraries:  HSHaXml >>pkg.conf
if "%GHCVER%"=="6.4.2"   %GHCPKG% register pkg.conf

goto Exit


rem -- Remove GHC package for HaXml --
:Remove
%GHCPKG% --remove-package HaXml

goto Exit


rem -- Build tools that come with HaXml --
:Tools
cd %SRC%\tools
for %%F in (Canonicalise  DtdToHaskell  MkOneOf  Validate  Xtract  CanonicaliseLazy  XtractLazy) DO %GHC% --make -cpp -i.. %%F -o %%F.exe
cd ..

goto Exit


rem -- All done --

:Exit
