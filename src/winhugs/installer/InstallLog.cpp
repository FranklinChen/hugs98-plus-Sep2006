#include "Header.h"

HANDLE hFile = INVALID_HANDLE_VALUE;
char* FileName = NULL;


void StartInstallLog(char* File)
{
	hFile = CreateFile(File, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	FileName = strdup(File);
}

void StopInstallLog(bool Delete)
{
	if (hFile != INVALID_HANDLE_VALUE)
	{
        CloseHandle(hFile);
		hFile = INVALID_HANDLE_VALUE;

		if (Delete)
			DeleteFile(FileName);
	}
	if (FileName != NULL)
	{
		free(FileName);
		FileName = NULL;
	}
}

void WriteInstallLog(char* Format, ...)
{
	if (hFile == INVALID_HANDLE_VALUE)
		return;

	char Buffer[MyMaxPath];

	va_list marker;
	va_start(marker, Format);
	wvsprintf(Buffer, Format, marker);

	strcat(Buffer, "\r\n");
	DWORD dw;
	WriteFile(hFile, Buffer, strlen(Buffer), &dw, NULL);
}



