
// all the data in these structures is intentionally leaked

struct Registry
{
	HKEY Root;
	char* Path;
};

struct File
{
	char* FileName;
	u32 CRC; //0 means no CRC
	int Size; //-1 means no size

	//flags for the program
	bool Modified;
};

struct Log
{
	int nRegistry;
	int nFile;
	Registry** Registrys;
	File** Files;
};

Log* ReadLog(char* Directory, char* FileName);
