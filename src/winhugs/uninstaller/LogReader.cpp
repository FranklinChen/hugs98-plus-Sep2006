
#include "header.h"
#include "LogReader.h"
#include "LinkedList.h"
#include <stdio.h>


char* GetLine(char* Data, int* Pos)
{
	char* Res = &Data[*Pos];
	if (Res[0] == 0)
		return NULL;

	bool Flag = false;
	for (int i = *Pos; ; i++)
	{
		char c = Data[i];
		if (c == '\r' || c == '\n')
		{
			Flag = true;
			Data[i] = 0;
		}
		else if (c == 0 || Flag == true)
		{
			*Pos = i;
			return Res;
		}
	}
}

void GetFields(char* Line, char** Fields)
{
	int FieldNo = 1;
	Fields[0] = Line;

	for (char* i = Line; *i != 0; i++)
	{
		if (*i == '\t')
		{
			*i = 0;
			Fields[FieldNo++] = i+1;
		}
	}
	Fields[FieldNo] = NULL;
	Fields[FieldNo+1] = NULL;
	Fields[FieldNo+2] = NULL;
}

File* ReadLogFile(char** Fields, char* Path, char* Pointer)
{
	char* s;
	File* f = new File;
	if (Fields[1][0] == '.' && Fields[1][1] == '\\')
	{
		strcpy(Pointer, &Fields[1][2]);
		f->FileName = strdup(Path);
	}
	else
		f->FileName = strdup(Fields[1]);

	f->Size = (Fields[2] ? atoi(Fields[2]) : -1);
	f->CRC = (Fields[3] ? strtoul(Fields[3], &s, 16) : 0);
	return f;
}

Registry* ReadLogReg(char** Fields)
{
	Registry* r = new Registry;
	if (strcmp(Fields[1], "HKEY_CLASSES_ROOT") == 0)
		r->Root = HKEY_CLASSES_ROOT;
	else if (strcmp(Fields[1], "HKEY_LOCAL_MACHINE") == 0)
		r->Root = HKEY_LOCAL_MACHINE;
	else
		r->Root = NULL;

	r->Path = strdup(Fields[2]);
	return r;
}

Log* ReadLog(char* Directory, char* FileName)
{
	HANDLE hFile = CreateFile(Directory, GENERIC_READ, 0, NULL,
		OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	if (hFile == INVALID_HANDLE_VALUE)
		return NULL;

	//now read the thing, line by line
	DWORD Size = GetFileSize(hFile, NULL);
	char* Data = new char[Size+1];
	DWORD dw;
	ReadFile(hFile, Data, Size, &dw, NULL);
	CloseHandle(hFile);

	if (dw != Size)
		return NULL;
	Data[Size] = 0;

	LinkedList* Regs = NULL;
	LinkedList* Files = NULL;

	//now the thing is in memory, lets have a go at parsing
	int Pos = 0;
	while(true)
	{
		char* Line = GetLine(Data, &Pos);
		if (Line == NULL) break;

		char* Fields[10];
		GetFields(Line, Fields);

		//ok, now parse a line
		if (Fields[0][0] == 0 || strcmp(Fields[0], "NOTE") == 0)
			; //discard, a comment
		else if (strcmp(Fields[0], "FILE") == 0)
			Files = NewLinkedList(ReadLogFile(Fields, Directory, FileName), Files);
		else if (strcmp(Fields[0], "REG") == 0)
			Regs = NewLinkedList(ReadLogReg(Fields), Regs);
	}

	//now lets make a Log structure
	Log* log = new Log;
	log->Files = (File**) LinkedListToArray(Files, &log->nFile);
	log->Registrys = (Registry**) LinkedListToArray(Regs, &log->nRegistry);

	return log;
}
