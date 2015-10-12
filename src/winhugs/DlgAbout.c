#include "Header.h"
#include "resrc1.h"
#include "Winmenu.h"

#include "prelude.h"
#include "storage.h"
#include "connect.h"

const int BmpWidth = 111;
const int BmpHeight = 112;
const int BmpTransparent = RGB(253,5,255);

LPCTSTR AboutText = 
    "Hugs 98: Based on the Haskell 98 standard\n"
    "Copyright © 1994-2006\n"
    "Bug reports to: mailto:hugs-bugs@haskell.org\n"
    "Website: http://www.haskell.org/hugs\n"
    "\n"
    "Please see the distribution for License and Credits info\n"
    "Version: ";

typedef struct _AboutData
{
    HIMAGELIST hImgList;
} AboutData;

INT_PTR CALLBACK AboutDlgProc(HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam);

void ShowAboutDialog(HWND hParent)
{
    DialogBox(hThisInstance, MAKEINTRESOURCE(ABOUTDLGBOX), hParent, AboutDlgProc);
}

INT_PTR CALLBACK AboutDlgProc(HWND hDlg, UINT Msg, WPARAM wParam, LPARAM lParam)
{
    switch (Msg) {
	case WM_INITDIALOG:
	    {
		HWND hRTF = GetDlgItem(hDlg, rtfAbout);
		AboutData* ad = malloc(sizeof(AboutData));
		HBITMAP hBmp;
		SetWindowLongPtr(hDlg, GWL_USERDATA, (LONG) ad);

	    CenterDialogInParent(hDlg);

	    ad->hImgList = ImageList_Create(BmpWidth, BmpHeight, ILC_COLOR24 | ILC_MASK, 1, 1);
	    hBmp = LoadBitmap(hThisInstance, MAKEINTRESOURCE(BMP_ABOUT));
	    ImageList_AddMasked(ad->hImgList, hBmp, BmpTransparent);
	    DeleteObject(hBmp);

	    SendMessage(hRTF, EM_AUTOURLDETECT, TRUE, 0);
	    SetWindowText(hRTF, AboutText);
	    SendMessage(hRTF, EM_SETSEL, -1, -1);
	    SendMessage(hRTF, EM_REPLACESEL, FALSE, (LPARAM) versionString);
	    SendMessage(hRTF, EM_SETEVENTMASK, 0, ENM_LINK);
	}
	return (INT_PTR)TRUE;

	case WM_PAINT:
	    {
		PAINTSTRUCT ps;
		AboutData* ad = (AboutData*) GetWindowLongPtr(hDlg, GWL_USERDATA);

		BeginPaint(hDlg, &ps);
		ImageList_Draw(ad->hImgList, 0, ps.hdc, 20, 25, ILD_TRANSPARENT);
		EndPaint(hDlg, &ps);
	    }
	    break;

	case WM_COMMAND:
	    switch (LOWORD(wParam)) {
		case IDOK: case IDCANCEL:
		    EndDialog(hDlg, TRUE);
		    return (INT_PTR)TRUE;
	    }
	    break;

	case WM_NOTIFY:
	    if (wParam == rtfAbout && ((LPNMHDR) lParam)->code == EN_LINK)
	    {
		TEXTRANGE tr;
		char Buffer[1000];
	        ENLINK* enl = (ENLINK*) lParam;

		if (enl->msg == WM_LBUTTONUP)
		{
		    tr.lpstrText = Buffer;
		    tr.chrg.cpMin = enl->chrg.cpMin;
		    tr.chrg.cpMax = enl->chrg.cpMax;

		    SendMessage(enl->nmhdr.hwndFrom, EM_GETTEXTRANGE, 0, (LPARAM) &tr);
		    ExecuteFile(Buffer);
		}
	    }
	    break;

	case WM_DESTROY:
	    {
		AboutData* ad = (AboutData*) GetWindowLongPtr(hDlg, GWL_USERDATA);
		ImageList_Destroy(ad->hImgList);
		free(ad);
	    }
    }
    return (INT_PTR)FALSE;
}
