@echo off
REM Setup for running on Neil Mitchell's machine
REM To run on other machines, install the appropriate things and
REM Change the environment variables at the top

REM REQUIRES:
REM     Internet Connection
REM     sed


REM --------------------------------------------------------------------------
REM Parameter section

REM xsltproc location (http://www.zlatkovic.com/libxml.en.html)
set XSLTPROC=xsltproc

REM Docbook location (no \ slashes, all must be /) (http://docbook.sourceforge.net/)
set DOCBOOK=d:/bin/docbook-xsl-1.60.1/htmlhelp/htmlhelp.xsl

REM Html Help Workshop (http://www.microsoft.com/downloads/details.aspx?familyid=00535334-c8a6-452f-9aa0-d597d16580cc&displaylang=en)
set HTMLHELP="c:\Program Files\HTML Help Workshop\hhc.exe"
REM --------------------------------------------------------------------------


mkdir users_guide_windows > nul
pushd users_guide_windows

REM First check the users guide is closed
if not exist hugs98.chm goto done
del hugs98.chm
if not exist hugs98.chm goto done

echo You have the .chm file still open, please close it first
goto finished

:done

echo Generating HTML
%XSLTPROC% %DOCBOOK% ../users_guide/users_guide.xml

copy ..\users_guide\*.png *.png
copy ..\users_guide\*.css *.css

echo Patching HTML
ren *.html *.h
for %%i in (*.h) do sed "s/<title>/<link rel='stylesheet' type='text\/css' href='hugs-ug.css'><title>/" %%i > %%itml

echo Generating CHM
%HTMLHELP% htmlhelp.hhp
ren htmlhelp.chm hugs98.chm


:finished

popd
echo Finished
