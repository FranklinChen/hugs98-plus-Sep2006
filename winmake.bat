@echo off
REM First run MSys
REM cd to your hugs distribution
REM Then type "make"
REM After that, run this file
REM Requires sed


echo REBUILDING EVERYTHING

set MyVC="%VS71COMNTOOLS%..\IDE\devenv.com"

%MyVC% src\msc\hugs.sln /build release
%MyVC% src\winhugs\winhugs.sln /build release
%MyVC% src\winhugs\installer\Installer.sln /build release
%MyVC% src\winhugs\uninstaller\Uninstaller.sln /build release

cd docs
call users-guide-windows.bat
cd ..

echo COPYING EVERYTHING

copy src\msc\ReleaseFfihugs\ffihugs.exe hugsdir\ffihugs.exe
copy src\msc\ffihugs.bat hugsdir\ffihugs.bat
copy src\msc\ReleaseHugs\hugs.exe hugsdir\hugs.exe
copy src\msc\ReleaseRunhugs\runhugs.exe hugsdir\runhugs.exe
copy src\WinHugs\Release\winhugs.exe hugsdir\winhugs.exe
mkdir hugsdir\docs 2> nul
copy docs\users_guide_windows\hugs98.chm hugsdir\docs\hugs98.chm
copy src\winhugs\uninstaller\Release hugsdir\uninstaller.exe
sed s/\n/\r\n/ Readme > hugsdir\readme.txt

mkdir release 2> nul

wzzip -ex -r -p release\winhugs.zip hugsdir
copy /b src\winhugs\installer\Release\installer.exe + release\winhugs.zip release\winhugs.exe


echo MAKING MINHUGS


mkdir mindir
mkdir mindir\docs
copy hugsdir\readme.txt mindir\readme.txt
copy hugsdir\uninstaller.exe mindir\uninstaller.exe
copy hugsdir\winhugs.exe mindir\winhugs.exe
copy hugsdir\docs\*.* mindir\docs\*.*

mkdir mindir\packages\base
mkdir mindir\packages\haskell98
mkdir mindir\packages\hugsbase
xcopy /y /s hugsdir\packages\base mindir\packages\base
xcopy /y /s hugsdir\packages\haskell98 mindir\packages\haskell98
xcopy /y /s hugsdir\packages\hugsbase mindir\packages\hugsbase

REM Remove Foreign
del mindir\packages\base\Data\ByteString.*
del mindir\packages\base\Data\ByteString\*.* /q
rmdir mindir\packages\base\Data\ByteString
del mindir\packages\base\System\Posix\*.* /q
del mindir\packages\base\System\Process\*.* /q
rmdir mindir\packages\base\System\Posix
rmdir mindir\packages\base\System\Process

wzzip -ex -r -p release\minhugs.zip mindir
copy /b src\winhugs\installer\Release\installer.exe + release\minhugs.zip release\minhugs.exe

echo DONE
