@echo off

REM First build up the command line to pass on in %s%
set s=
:begin
if '%1' == '' goto done
set s=%s% %1
shift
goto begin
:done


REM Now figure out what to do
if not "%VS71COMNTOOLS%" == "" call "%VS71COMNTOOLS%vsvars32.bat" > nul


cl %s%
