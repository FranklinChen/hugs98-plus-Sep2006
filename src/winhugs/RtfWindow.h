

void RtfWindowInit(HWND hNewRTF);
void RtfWindowUpdateFont();
void RtfWindowTextColor(int Color);
void RtfWindowPutChar(char c);
void RtfWindowPutChars(char* s);
BOOL RtfNotify(HWND hDlg, NMHDR* nmhdr);
int RtfWindowCanCutCopy();
void RtfWindowClipboard(UINT Msg);

void RtfWindowClear();
void RtfWindowDelete();
void RtfWindowHistory(int Delta);
void RtfWindowSelectAll();
void RtfWindowStartOutput();
void RtfWindowStartInput();
