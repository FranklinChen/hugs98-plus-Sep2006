@echo off

rem  A simple configure script for people who don't have /bin/sh and related
rem  tools.

echo Copying .\Makefile, .\options.h and .\config.h to ..
copy .\Makefile  ..
copy .\options.h ..
copy .\config.h  ..

echo You may need to edit ..\Makefile and ..\config.h to suit the
echo peculiarities of your machine.

echo You may also choose to edit ..\options.h to suit your personal preferences.
