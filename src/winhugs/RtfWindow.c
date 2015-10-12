#include "Header.h"
#include <stdio.h>
#include "Winhugs.h"

// have a max of 100Kb in the scroll window
// old hugs was about 64Kb
#define MAXIMUM_BUFFER   100000

// Buffer the RTF Window Handle
// Only allow one RTF Window at a time
HWND hRTF;
BOOL PuttingChar = FALSE;
DWORD Length = 0;
DWORD OutputStart;

typedef struct _Format
{
    int ForeColor;
    int BackColor;
    BOOL Bold;
    BOOL Italic;
    BOOL Underline;
} Format;

BOOL FormatChanged = FALSE;
Format DefFormat = {BLACK, WHITE, FALSE, FALSE, FALSE};
Format BufFormat;
Format NowFormat;

HANDLE hMutex;


void RtfWindowInit(HWND hNewRTF)
{
    CHARFORMAT cf;
    hRTF = hNewRTF;

    //make it all protected
    SendMessage(hRTF, EM_SETEVENTMASK, 0,
	ENM_PROTECTED | ENM_LINK | ENM_KEYEVENTS | ENM_SELCHANGE);
    cf.cbSize = sizeof(cf);
    cf.dwEffects = CFE_PROTECTED;
    cf.dwMask = CFM_PROTECTED;
    SendMessage(hRTF, EM_SETCHARFORMAT, SCF_ALL, (LPARAM) &cf);

    // Allow them 1 million characters
    // the system will sort out overflows later
    SendMessage(hRTF, EM_LIMITTEXT, 1000000, 0);

    // Default formatting information
    BufFormat = DefFormat;
    NowFormat = DefFormat;

    // And syncronisation stuff
    hMutex = CreateMutex(NULL, FALSE, NULL);

    //update the font
    RtfWindowUpdateFont();
}

void RtfWindowUpdateFont()
{
    CHARFORMAT cf;
    RegistryReadFont(&cf);
    SendMessage(hRTF, EM_SETCHARFORMAT, SCF_ALL, (LPARAM) &cf);
}

int RtfWindowTextLength()
{
    GETTEXTLENGTHEX gtl;
    gtl.codepage = CP_ACP;
    gtl.flags = GTL_DEFAULT;
    return SendMessage(hRTF, EM_GETTEXTLENGTHEX, (WPARAM) &gtl, 0);
}

// return a bit mask of DROPEFFECT_NONE, DROPEFFECT_COPY, DROPEFFECT_MOVE
int RtfWindowCanCutCopy()
{
    DWORD Start, End;
    SendMessage(hRTF, EM_GETSEL, (WPARAM) &Start, (WPARAM) &End);
    if (Start == End)
	return DROPEFFECT_NONE;
    else if (Start >= Length)
	return DROPEFFECT_COPY | DROPEFFECT_MOVE;
    else
	return DROPEFFECT_COPY;
}

void RtfWindowClear()
{
    CHARRANGE cr;
    int Lines = SendMessage(hRTF, EM_GETLINECOUNT, 0, 0);
    int ThisLine = SendMessage(hRTF, EM_LINEINDEX, Lines-1, 0);

    SendMessage(hRTF, EM_EXGETSEL, 0, (LPARAM) &cr);
    SendMessage(hRTF, EM_SETSEL, 0, ThisLine);
    PuttingChar = TRUE;
    SendMessage(hRTF, EM_REPLACESEL, FALSE, (LPARAM) "");
    PuttingChar = FALSE;

    cr.cpMax -= ThisLine;
    cr.cpMin -= ThisLine;

    Length -= ThisLine;
    if (cr.cpMin < 0)
        SendMessage(hRTF, EM_SETSEL, Length, Length);
    else
	SendMessage(hRTF, EM_EXSETSEL, 0, (LPARAM) &cr);
}

void RtfWindowDelete()
{
    SendMessage(hRTF, EM_REPLACESEL, FALSE, (LPARAM) "");
}

void RtfWindowHistory(int Delta)
{
    LPCSTR x = GetHistory(Delta);
    if (x == NULL)
	MessageBeep((UINT) -1);
    else
	RtfWindowSetCommand(x);
}

void RtfWindowSelectAll()
{
    SendMessage(hRTF, EM_SETSEL, 0, -1);
}

BOOL RtfNotify(HWND hDlg, NMHDR* nmhdr)
{
    if (nmhdr->code == EN_PROTECTED && !PuttingChar) {
	//block
	ENPROTECTED* enp = (ENPROTECTED*) nmhdr;
	CHARRANGE cr;
	int TextLen = RtfWindowTextLength();
	BOOL Reset = FALSE, Disallow = FALSE;

	// just let it go ahead anyway
	if (enp->msg == WM_COPY)
	    return FALSE;

	// they hit backspace
	if (enp->wParam == VK_BACK) {
	    if ((DWORD) enp->chrg.cpMin < Length ||
		((DWORD) enp->chrg.cpMin == Length &&
		 enp->chrg.cpMin == enp->chrg.cpMax)) {
		Reset = TRUE;
		Disallow = TRUE;
	    }
	} else if ((DWORD) enp->chrg.cpMin < Length) {
	    Reset = TRUE;
	    Disallow = (enp->wParam == VK_DELETE);
	}

	if (Reset) {
	    cr.cpMin = TextLen;
	    cr.cpMax = cr.cpMin;
	    SendMessage(hRTF, EM_EXSETSEL, 0, (LPARAM) &cr);
	}

	// we don't want to paste rich text, as that makes it look weird
	// so send only plain text paste commands
	if ((enp->msg == WM_PASTE) && !Disallow) {
	    LPTSTR Buffer = NULL;
	    Disallow = TRUE;
	    if (IsClipboardFormatAvailable(CF_TEXT) &&
		OpenClipboard(hThisWindow)) {
		HGLOBAL hGlb; 
		LPTSTR str; 

	        if ((hGlb = GetClipboardData(CF_TEXT)) != NULL &&
		    (str = GlobalLock(hGlb)) != NULL) {
		    Buffer = strdup(str);
		    GlobalUnlock(hGlb);
		}
		CloseClipboard();
	    }

	    if (Buffer != NULL) {
		// strip trailing new line characters
		int i;
		for (i = strlen(Buffer)-1;
		    i >= 0 && (Buffer[i] == '\r' || Buffer[i] == '\n');
		    i--)
		    Buffer[i] = 0;
		SendMessage(hRTF, EM_REPLACESEL, FALSE, (LPARAM)Buffer);
		free(Buffer);
	    }
	}

	SetWindowLong(hDlg, DWL_MSGRESULT, (Disallow ? 1 : 0));
	return TRUE;
    } else if (nmhdr->code == EN_LINK) {
	// should really fire on up
	// but that screws up the cursor position

	ENLINK* enl = (ENLINK*) nmhdr;
	if (enl->msg == WM_LBUTTONDOWN) {
	    TEXTRANGE tr;
	    char Buffer[1000];
	    tr.lpstrText = Buffer;
	    tr.chrg.cpMin = enl->chrg.cpMin;
	    tr.chrg.cpMax = enl->chrg.cpMax;

	    SendMessage(hRTF, EM_GETTEXTRANGE, 0, (LPARAM) &tr);
	    ExecuteFile(Buffer);

	    SetWindowLong(hDlg, DWL_MSGRESULT, 1);
	    return TRUE;
	}
    } else if (nmhdr->code == EN_MSGFILTER) {
	MSGFILTER* mf = (MSGFILTER*) nmhdr;
	if (mf->msg == WM_CHAR && Running) {
	    WinHugsReceiveC(mf->wParam == '\r' ? '\n' : mf->wParam);
	    SetWindowLong(hDlg, DWL_MSGRESULT, 1);
	    return TRUE;
	} else if (Running && mf->msg == WM_KEYDOWN) {
	    SetWindowLong(hDlg, DWL_MSGRESULT, 1);
	    return TRUE;
	} else if (mf->msg == WM_KEYDOWN && !Running) {
	    BOOL History = (mf->wParam == VK_UP || mf->wParam == VK_DOWN);
	    if (History && (mf->lParam & (1 << 24))) {
		CHARRANGE cr;
		SendMessage(hRTF, EM_EXGETSEL, 0, (LPARAM) &cr);
		if ((DWORD) cr.cpMin >= Length) {
		    RtfWindowHistory(mf->wParam == VK_UP ? -1 : +1);
		    SetWindowLong(hDlg, DWL_MSGRESULT, 1);
		    return TRUE;
		}
	    } else if (mf->wParam == VK_RETURN) {
		char Buffer[1000];
		RtfWindowGetCommand(Buffer);
		FireCommandDelay(Buffer);
		SetWindowLong(hDlg, DWL_MSGRESULT, 1);
		return TRUE;
	    } else if (mf->wParam == VK_HOME) {
		CHARRANGE cr;
		SendMessage(hRTF, EM_EXGETSEL, 0, (LPARAM) &cr);
		if ((DWORD) cr.cpMin > Length) {
		    SHORT n = GetKeyState(VK_SHIFT);
		    BOOL Shift = (n & (1 << 16));

		    SetWindowLong(hDlg, DWL_MSGRESULT, 1);
		    cr.cpMin = Length;
		    cr.cpMax = (Shift ? cr.cpMax : Length);
		    SendMessage(hRTF, EM_EXSETSEL, 0, (LPARAM) &cr);
		    SetWindowLong(hDlg, DWL_MSGRESULT, 1);
		    return TRUE;
		}
	    }
	}
    } else if (nmhdr->code == EN_SELCHANGE) {
	EnableButtons();
    }

    return FALSE;
}

// Respond to a clipboard message
// WM_PASTE, WM_COPY, WM_CUT
void RtfWindowClipboard(UINT Msg)
{
    SendMessage(hRTF, Msg, 0, 0);
}

// NULL means freeze in the existing command
void RtfWindowSetCommand(LPCSTR Command)
{
    SendMessage(hRTF, EM_SETSEL, Length, RtfWindowTextLength());
    PuttingChar = TRUE;
    SendMessage(hRTF, EM_REPLACESEL, FALSE, (LPARAM) Command);
    PuttingChar = FALSE;
}

void RtfWindowGetCommand(LPSTR Command)
{
    TEXTRANGE tr;

    tr.lpstrText = Command;
    tr.chrg.cpMin = Length;
    tr.chrg.cpMax = RtfWindowTextLength();

    if (tr.chrg.cpMin == tr.chrg.cpMax)
	Command[0] = 0;
    else
	SendMessage(hRTF, EM_GETTEXTRANGE, 0, (LPARAM) &tr);
}

/////////////////////////////////////////////////////////////////////
// BUFFERING AND OUTPUT
/////////////////////////////////////////////////////////////////////

const int BufSize = 995;
char Buf[1000];
int BufPos = 0; // where to write out in the buffer
int BufLen = 0; // how much of the buffer is useful
int OutputPos = 0; // how much to delete of the existing thing
BOOL IsTimer = FALSE;

// buffer to hold an escape character
BOOL InEscBuf = FALSE;
const int EscBufSize = 100;
char EscBuf[100];
int EscBufPos = 0;

void EnsureTimer()
{
    if (!IsTimer) {
	IsTimer = TRUE;
	SetTimer(GetParent(hRTF), 666, 100, NULL);
    }
}

void DestTimer()
{
    KillTimer(GetParent(hRTF), 666);
    IsTimer = FALSE;
}

void FixCharFormat(CHARFORMAT2* cf)
{
	if (cf->crTextColor == BLACK)
		cf->dwEffects |= CFE_AUTOCOLOR;
	if (cf->crBackColor == WHITE)
		cf->dwEffects |= CFE_AUTOBACKCOLOR;
}

void WriteBuffer(LPCTSTR s, int Len)
{
    CHARRANGE cr;
    CHARFORMAT2 cf;
    Length = RtfWindowTextLength();

    cr.cpMin = max(OutputStart, Length + OutputPos);
    cr.cpMax = cr.cpMin + BufLen;
    SendMessage(hRTF, EM_EXSETSEL, 0, (LPARAM) &cr);

    cf.cbSize = sizeof(cf);
    cf.dwMask = CFM_COLOR | CFM_BACKCOLOR | CFM_BOLD | CFM_ITALIC | CFM_UNDERLINE;
    cf.dwEffects = 0;
    cf.crTextColor = BufFormat.ForeColor;
    cf.crBackColor = BufFormat.BackColor;
    cf.dwEffects = (BufFormat.Bold ? CFE_BOLD : 0) |
		   (BufFormat.Italic ? CFE_ITALIC : 0) |
		   (BufFormat.Underline ? CFE_UNDERLINE : 0);
	FixCharFormat(&cf);
    SendMessage(hRTF, EM_SETCHARFORMAT, SCF_SELECTION, (LPARAM) &cf);
    // setcharformat seems to screw up the current selection!

    SendMessage(hRTF, EM_EXSETSEL, 0, (LPARAM) &cr);
    PuttingChar = TRUE;
    SendMessage(hRTF, EM_REPLACESEL, FALSE, (LPARAM) s);
    PuttingChar = FALSE;

    Length = RtfWindowTextLength();
    if (Length > MAXIMUM_BUFFER) {
	LPCSTR Blank = "";
	CHARRANGE cr;

	SendMessage(hRTF, EM_HIDESELECTION, TRUE, 0);

	cr.cpMin = 0;
	cr.cpMax = (Length - MAXIMUM_BUFFER) + (MAXIMUM_BUFFER / 4);
	SendMessage(hRTF, EM_EXSETSEL, 0, (LPARAM) &cr);

	PuttingChar = TRUE;
	SendMessage(hRTF, EM_REPLACESEL, FALSE, (LPARAM) Blank);
	PuttingChar = FALSE;

	cr.cpMin = -1;
	cr.cpMax = -1;
	SendMessage(hRTF, EM_EXSETSEL, 0, (LPARAM) &cr);

	SendMessage(hRTF, EM_HIDESELECTION, FALSE, 0);

	Length = RtfWindowTextLength();
    }
}

void FlushBuffer(BOOL Force)
{
    DWORD Res = WaitForSingleObject(hMutex, (Force ? INFINITE : 0));

    if (Res != WAIT_OBJECT_0)
	return; //you did not win

    if (BufLen != 0) {
	Buf[BufLen] = 0;
	Buf[BufLen+1] = 0;
	WriteBuffer(Buf, BufLen);
	OutputPos = BufPos - BufLen;
	BufPos = 0;
	BufLen = 0;
    }

    ReleaseMutex(hMutex);
}

void RtfWindowFlushBuffer()
{
    FlushBuffer(TRUE);
}

BOOL ParseEscapeCode(Format* f)
{
    int AnsiColor[8] = {BLACK, RED, GREEN, YELLOW, BLUE,
			MAGENTA, CYAN, WHITE};
    char* s;
    int i;

    EscBuf[EscBufPos] = 0;
    if (EscBuf[0] != '[')
	return FALSE;

    s = &EscBuf[1];
    for (i = 1; i <= EscBufPos; i++) {
	if (EscBuf[i] == ';')
	    EscBuf[i] = 0;

	if (EscBuf[i] == 0) {
	    int Val = atoi(s);
	    s = &EscBuf[i+1];

	    if (Val == 0)
		*f = DefFormat;
	    else if (Val == 1)
		f->Bold = TRUE;
	    else if (Val == 4)
		f->Underline = TRUE;
	    else if (Val >= 30 && Val <= 37)
		f->ForeColor = AnsiColor[Val - 30];
	    else if (Val >= 40 && Val <= 47)
		f->BackColor = AnsiColor[Val - 40];
	}
    }
    return TRUE;
}

// need to copy from s to Buf
void AddToBuffer(LPCTSTR s)
{
    if (FormatChanged) {
	if (NowFormat.BackColor != BufFormat.BackColor ||
	    NowFormat.ForeColor != BufFormat.ForeColor ||
	    NowFormat.Bold      != BufFormat.Bold      ||
	    NowFormat.Underline != BufFormat.Underline ||
	    NowFormat.Italic    != BufFormat.Italic    )
	{
	    FlushBuffer(TRUE);
	    BufFormat = NowFormat;
	}
	FormatChanged = FALSE;
    }

    if (InEscBuf) {
	for (; *s != 0; s++) {
	    if (*s == 'm') {
		Format f = NowFormat;
		if (ParseEscapeCode(&f)) {
		    FormatChanged = TRUE;
		    NowFormat = f;
		}
		InEscBuf = FALSE;
		AddToBuffer(s+1);
		return;
	    } else if ((*s >= '0' && *s <= '9') ||
		(*s == ';') || (*s == '[')) {
		EscBuf[EscBufPos++] = *s;
		EscBufPos = min(EscBufPos, EscBufSize);
	    } else {
		InEscBuf = FALSE;
		AddToBuffer(EscBuf);
		break;
	    }
	}
    }

    for (; *s != 0; s++) {
	if (*s == '\b') {
	    if (BufPos == 0) {
		OutputPos--;
	    } else
		BufPos--;
	} else if (*s == 27) {
	    InEscBuf = TRUE;
	    EscBufPos = 0;
	    AddToBuffer(s+1);
	    return;
	} else {
	    if (BufLen >= BufSize)
		FlushBuffer(TRUE);
	    Buf[BufPos++] = *s;
	    BufLen = max(BufLen, BufPos);
	}
    }

    EnsureTimer();
}

void RtfWindowTimer()
{
    // if you are doing useful work, why die?
    if (BufLen == 0)
	DestTimer();
    FlushBuffer(FALSE);
}

void RtfWindowPutS(LPCTSTR s)
{
    AddToBuffer(s);
}

void RtfEchoCommand(LPCTSTR s)
{
    RtfWindowPutS(s);
    RtfWindowPutS("\n");
}

void RtfWindowStartOutput()
{
    RtfWindowPutS("\n");
    RtfWindowFlushBuffer();
    BufFormat = DefFormat;
    NowFormat = DefFormat;
    OutputStart = RtfWindowTextLength();
}

void RtfWindowStartInput()
{
    CHARRANGE cr;
    CHARFORMAT cf;
    cf.cbSize = sizeof(cf);
    cf.dwMask = CFM_COLOR;
    cf.dwEffects = 0;
    cf.crTextColor = BLACK;

    cr.cpMin = Length;
    cr.cpMax = -1;
    SendMessage(hRTF, EM_EXSETSEL, 0, (LPARAM) &cr);

    SendMessage(hRTF, EM_SETCHARFORMAT, SCF_SELECTION, (LPARAM) &cf);

    cr.cpMax = cr.cpMin;
    SendMessage(hRTF, EM_EXSETSEL, 0, (LPARAM) &cr);
}

int WinHugsColor(int Color)
{
    int PrevColor = NowFormat.ForeColor;
    FormatChanged = TRUE;
    NowFormat = DefFormat;
    NowFormat.ForeColor = Color;
    InEscBuf = FALSE;
    return PrevColor;
}

/////////////////////////////////////////////////////////////////////
// IO REDIRECTORS
/////////////////////////////////////////////////////////////////////

void WinHugsHyperlink(const char* msg)
{
    CHARFORMAT2 cf2;
    FlushBuffer(TRUE);

    cf2.cbSize = sizeof(cf2);
    cf2.dwMask = CFM_LINK;
    cf2.dwEffects = CFE_LINK;

    SendMessage(hRTF, EM_SETCHARFORMAT, SCF_SELECTION, (LPARAM) &cf2);
    SendMessage(hRTF, EM_REPLACESEL, FALSE, (LPARAM) msg);
    Length += strlen(msg);
    cf2.dwEffects = 0;
    SendMessage(hRTF, EM_SETCHARFORMAT, SCF_SELECTION, (LPARAM) &cf2);
}
