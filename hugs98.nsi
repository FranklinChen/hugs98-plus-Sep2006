; First attempt at an NSIS (http://nsis.sourceforge.net/) installer
; script for Hugs 98.

!ifndef VERSION
	!ifdef MAJOR_RELEASE
		!define /date VERSION "%b%Y"
	!else
		!define /date VERSION "%Y%m%d"
	!endif
!endif

!define HUGS_KEY "Software\Haskell\Hugs${VERSION}"
!define WINHUGS_KEY "Software\Haskell\Hugs\WinHugs${VERSION}"
!define UNINSTALL_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\Hugs98"

SetCompressor lzma

Name "Hugs98"
OutFile "hugs98-${VERSION}.exe"
InstallDir "$PROGRAMFILES\Hugs98"
InstallDirRegKey HKLM "${HUGS_KEY}" "Install_Dir"
XPStyle on

Page directory
Page instfiles
 
UninstPage uninstConfirm
UninstPage instfiles

Section

	SetOutPath "$INSTDIR"

	File "Readme"
	File "License"
	File "Credits"

	File "src\hugs.exe"
	File "src\ffihugs.exe"
	File "src\runhugs.exe"

	File /r /x *.c "hugsdir\libraries"
	File /r /x *.c "hugsdir\packages"
	File /r /x *.c "hugsdir\programs"
	File /r "hugsdir\oldlib"
	File /r "hugsdir\demos"
	File /r "hugsdir\include"

	WriteRegStr HKLM "${HUGS_KEY}" "Install_Dir" "$INSTDIR"

	WriteRegStr HKLM "${UNINSTALL_KEY}" "DisplayName" "Hugs98"
	WriteRegStr HKLM "${UNINSTALL_KEY}" "UninstallString" '"$INSTDIR\uninstall.exe"'
	WriteRegDWORD HKLM "${UNINSTALL_KEY}" "NoModify" 1
	WriteRegDWORD HKLM "${UNINSTALL_KEY}" "NoRepair" 1
	WriteUninstaller "uninstall.exe"

	CreateDirectory "$SMPROGRAMS\Hugs98"
	CreateShortCut "$SMPROGRAMS\Hugs98\Uninstall.lnk" "$INSTDIR\uninstall.exe"
	CreateShortCut "$SMPROGRAMS\Hugs98\Hugs.lnk" "$INSTDIR\hugs.exe"

SectionEnd

Section "Uninstall"

	DeleteRegKey HKLM "${UNINSTALL_KEY}"
	DeleteRegKey HKLM "${HUGS_KEY}"

	RMDir /r "$SMPROGRAMS\Hugs98"
	RMDir /r "$INSTDIR"

SectionEnd
