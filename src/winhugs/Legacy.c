/* --------------------------------------------------------------------------
 * WinBrows.c:	José Enrique Gallardo Ruiz, Feb 1999
 *              With modifications by mpj/adr for Hugs, 1995-97
 *
 * The Hugs 98 system is Copyright (c) JosÂ€ Enrique Gallardo, Mark P Jones,
 * Alastair Reid, the Yale Haskell Group, and the OGI School of
 * Science & Engineering at OHSU, 1994-2003, All rights reserved.  It is
 * distributed as free software under the license in the file "License",
 * which is included in the distribution.
 * ------------------------------------------------------------------------*/

// NEIL:
//    I don't much like the look of this and I'm not going anywhere near it!
//    Hopefully a hoogle solution can be done in the future
//    Modified minimally to get it working
//    Moved out of WinHugs.c and into separate module, a bit of massage to make that work

#include "Header.h"
#include "Winhugs.h"
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "machdep.h"
#include "strutil.h"
#include "script.h"
#include "opts.h"
#include "output.h"
#include "evaluator.h"
#include "Winmenu.h"

# define CMDdata(w,l)  (HIWORD(w))	/* decoding WM_COMMAND message     */
# define CMDitem(w,l)  (LOWORD(w))
# define CMDhwnd(w,l)  ((HWND)(l))

//#include "winhugs.h"
#include "Header.h"

HWND hWndClasses = NULL;

#define DlgSendMessage(h,c,w,l)  SendMessage((h),(c),MAKEWPARAM(w,(HIWORD(l))),(LOWORD(l)))

/* Draws a bitmap on a DC */
VOID DrawBitmap(HDC hDC, HBITMAP hBitmap, UINT left, UINT top)
{
    HBITMAP hOldBitmap;
    BITMAP bm;
    HDC hDCMemory;

    GetObject(hBitmap, sizeof(BITMAP), &bm);
    hDCMemory = CreateCompatibleDC(hDC);
    hOldBitmap = SelectObject(hDCMemory, hBitmap);
    BitBlt(hDC, left, top, bm.bmWidth, bm.bmHeight, hDCMemory, 0, 0,
	   SRCCOPY);
    SelectObject(hDCMemory, hOldBitmap);
    DeleteDC(hDCMemory);
}

BOOL CALLBACK SetDialogFontProc(HWND hwndChild, LPARAM hFont)
{
    SendMessage(hwndChild, WM_SETFONT, (WPARAM) hFont, (DWORD) TRUE);
    return TRUE;
}

/* Call this function in WM_INITDIALOG to set a font for every control in the dialog */
VOID SetDialogFont(HWND hDlg, HFONT hFont)
{

    EnumChildWindows(hDlg, SetDialogFontProc, (LPARAM) hFont);
}

/*************************************************************************
 *
 * ChangeBitmapColorDC()
 *
 * This function makes all pixels in the given DC that have the
 * color rgbOld have the color rgbNew.  This function is used by
 * ChangeBitmapColor().
 *
 * Parameters:
 *
 * HDC hdcBM        - Memory DC containing bitmap
 * LPBITMAP lpBM    - Long pointer to bitmap structure from hdcBM
 * COLORREF rgbOld  - Source color
 * COLORREF rgbNew  - Destination color
 *
 * Return value: none.
 *
 * History:   Date      Author      Reason
 *            6/10/91   CKindel     Created
 *            1/23/92   MarkBad     Added big nifty comments which explain
 *                                  how this works, split bitmap graying
 *                                  code out
 *
 *************************************************************************/

static VOID ChangeBitmapColorDC(HDC hdcBM, LPBITMAP lpBM,
		COLORREF rgbOld, COLORREF rgbNew)
{
    HDC hdcMask;
    HBITMAP hbmMask, hbmOld;
    HBRUSH hbrOld;

    if (!lpBM)
	return;

    /* if the bitmap is mono we have nothing to do */
    if (lpBM->bmPlanes == 1 && lpBM->bmBitsPixel == 1)
	return;

    /* To perform the color switching, we need to create a monochrome
       // "mask" which is the same size as our color bitmap, but has all
       // pixels which match the old color (rgbOld) in the bitmap set to 1.
       //
       // We then use the ROP code "DSPDxax" to Blt our monochrome
       // bitmap to the color bitmap.  "D" is the Destination color
       // bitmap, "S" is the source monochrome bitmap, and "P" is the
       // selected brush (which is set to the replacement color (rgbNew)).
       // "x" and "a" represent the XOR and AND operators, respectively.
       //
       // The DSPDxax ROP code can be explained as having the following
       // effect:
       //
       // "Every place the Source bitmap is 1, we want to replace the
       // same location in our color bitmap with the new color.  All
       // other colors we leave as is."
       //
       // The truth table for DSPDxax is as follows:
       //
       //       D S P Result
       //       - - - ------
       //       0 0 0   0
       //       0 0 1   0
       //       0 1 0   0
       //       0 1 1   1
       //       1 0 0   1
       //       1 0 1   1
       //       1 1 0   0
       //       1 1 1   1
       //
       // (Even though the table is assuming monochrome D (Destination color),
       // S (Source color), & P's (Pattern color), the results apply to color
       // bitmaps also).
       //
       // By examining the table, every place that the Source is 1
       // (source bitmap contains a 1), the result is equal to the
       // Pattern at that location.  Where S is zero, the result equals
       // the Destination.
       //
       // See Section 11.2 (page 11-4) of the "Reference -- Volume 2" for more
       // information on the Termary Raster Operation codes.
     */

    if (hbmMask =
	CreateBitmap(lpBM->bmWidth, lpBM->bmHeight, 1, 1, NULL)) {
	if (hdcMask = CreateCompatibleDC(hdcBM)) {
	    /* Select th mask bitmap into the mono DC */
	    hbmOld = SelectObject(hdcMask, hbmMask);

	    /* Create the brush and select it into the source color DC  */
	    /* this is our "Pattern" or "P" color in our DSPDxax ROP.   */
	    hbrOld = SelectObject(hdcBM,
			  CreateSolidBrush(rgbNew));

	    /* To create the mask, we will use a feature of BitBlt -- when
	       // converting from Color to Mono bitmaps, all Pixels of the
	       // background colors are set to WHITE (1), and all other pixels
	       // are set to BLACK (0).  So all pixels in our bitmap that are
	       // rgbOld color, we set to 1.
	     */

	    SetBkColor(hdcBM, rgbOld);
	    BitBlt(hdcMask, 0, 0, lpBM->bmWidth,
		   lpBM->bmHeight, hdcBM, 0, 0, SRCCOPY);

	    /* Where the mask is 1, lay down the brush, where it */
	    /* is 0, leave the destination.                      */

#define RGBBLACK     	RGB(0,0,0)
#define RGBWHITE     	RGB(255,255,255)
#define DSa       	0x008800C6L
#define DSo       	0x00EE0086L
#define DSx       	0x00660045L
#define DSPDxax   	0x00E20746L

	    SetBkColor(hdcBM, RGBWHITE);
	    SetTextColor(hdcBM, RGBBLACK);

	    BitBlt(hdcBM, 0, 0, lpBM->bmWidth, lpBM->bmHeight,
		   hdcMask, 0, 0, DSPDxax);

	    SelectObject(hdcMask, hbmOld);

	    hbrOld = SelectObject(hdcBM, hbrOld);
	    DeleteObject(hbrOld);

	    DeleteDC(hdcMask);
	} else
	    return;

	DeleteObject(hbmMask);
    } else
	return;
}

VOID MapBitmap(HBITMAP hbmSrc, COLORREF rgbOld, COLORREF rgbNew)
{
    HDC hDC, hdcMem;
    BITMAP bmBits;

    if (hDC = GetDC(NULL)) {
	if (hdcMem = CreateCompatibleDC(hDC)) {

	    /* Get the bitmap struct needed by ChangeBitmapColorDC() */
	    GetObject(hbmSrc, sizeof(BITMAP),
		  (LPSTR) & bmBits);

	    /* Select our bitmap into the memory DC */
	    hbmSrc = SelectObject(hdcMem, hbmSrc);

	    /* Translate the sucker */
	    ChangeBitmapColorDC(hdcMem, &bmBits, rgbOld,
			rgbNew);

	    /* Unselect our bitmap before deleting the DC */
	    hbmSrc = SelectObject(hdcMem, hbmSrc);

	    DeleteDC(hdcMem);
	}
	ReleaseDC(NULL, hDC);
    }
}

/* Default colors used to map the DIB colors	*/
/* to the current system colors:                */
#define MAPPED_BUTTONTEXT      (RGB(000,000,000))	/* black          */
#define MAPPED_BUTTONSHADOW    (RGB(128,000,000))	/* dark red       */
#define MAPPED_BUTTONFACE      (RGB(255,000,255))	/* bright magenta */
#define MAPPED_BUTTONHILIGHT   (RGB(255,255,255))	/* white          */

/* Executes the dialog with DlgId identifier using lpDlgProc */
BOOL ExecDialog(HINSTANCE hInstance, WORD DlgId, DLGPROC lpDlgProc)
{
    return (BOOL) (DialogBox
	       (hInstance, MAKEINTRESOURCE(DlgId), GetFocus(),
	    lpDlgProc));
}

/* Loads a bitmap and maps system colors */
HBITMAP LoadMappedBitmap(HINSTANCE hInstance, LPCSTR BitmapName)
{
    HBITMAP hBitmap;

    hBitmap = LoadBitmap(hInstance, BitmapName);
    MapBitmap(hBitmap, MAPPED_BUTTONHILIGHT,
	  GetSysColor(COLOR_BTNHIGHLIGHT));
    MapBitmap(hBitmap, MAPPED_BUTTONTEXT, GetSysColor(COLOR_BTNTEXT));
    MapBitmap(hBitmap, MAPPED_BUTTONSHADOW,
	  GetSysColor(COLOR_BTNSHADOW));
    MapBitmap(hBitmap, MAPPED_BUTTONFACE, GetSysColor(COLOR_BTNFACE));

    return hBitmap;
}

HFONT DefaultFont()
{
    return GetStockObject(DEFAULT_GUI_FONT);
}

/* --------------------------------------------------------------------------
 * Browse dialog boxes:
 * ------------------------------------------------------------------------*/

static VOID SetClass Args((HWND, Class));
static VOID SetName Args((HWND, UINT, List));
static VOID SetTycon Args((HWND, UINT, List));

/* When a class changes to currClass get new list of instances, */
/* members and contexts for the new class                       */
static local VOID SetClass(HWND hDlg, Class currClass)
{
    INT i;
    List instances, members;

    /* Update list of instances */
    SendDlgItemMessage(hDlg, LB_INSTANCES, LB_RESETCONTENT, 0, 0L);

    /* Clear the redraw flag */
    SendDlgItemMessage(hDlg, LB_INSTANCES, WM_SETREDRAW, FALSE, 0L);

    for (instances = cclass(currClass).instances; !isNull(instances);
	 instances = tl(instances)) {
	if (!isNull(instances)) {
	    SendDlgItemMessage(hDlg, LB_INSTANCES,
		       LB_ADDSTRING, 0,
		       (LONG) (LPSTR) hd(instances));
	}
	SendDlgItemMessage(hDlg, LB_INSTANCES, LB_SETCURSEL, 0,
		   0L);
    }

    /* Set the redraw flag and force repaint. */
    SendDlgItemMessage(hDlg, LB_INSTANCES, WM_SETREDRAW, TRUE, 0L);
    InvalidateRect(GetDlgItem(hDlg, LB_INSTANCES), NULL, TRUE);

    /* Update list of members */

    /* Clear the redraw flag */
    SendDlgItemMessage(hDlg, LB_MEMBERS, WM_SETREDRAW, FALSE, 0L);

    SendDlgItemMessage(hDlg, LB_MEMBERS, LB_RESETCONTENT, 0, 0L);
    if (cclass(currClass).numMembers > 0) {
	for (members = cclass(currClass).members, i = 0;
	     i < cclass(currClass).numMembers;
	     members = tl(members), i++) {
	    SendDlgItemMessage(hDlg, LB_MEMBERS, LB_ADDSTRING,
		       0, (LONG) (LPSTR) hd(members));
	}
	SendDlgItemMessage(hDlg, LB_MEMBERS, LB_SETCURSEL, 0, 0L);
    }

    /* Set the redraw flag and force repaint. */
    SendDlgItemMessage(hDlg, LB_MEMBERS, WM_SETREDRAW, TRUE, 0L);
    InvalidateRect(GetDlgItem(hDlg, LB_MEMBERS), NULL, TRUE);

    /* Update context */
    SendDlgItemMessage(hDlg, LB_CONTEXT, LB_RESETCONTENT, 0, 0L);
    if (nonNull(cclass(currClass).supers)) {
	printContext(stdstr, cclass(currClass).supers);
	fprintf(stdstr, "\n");
	SendDlgItemMessage(hDlg, LB_CONTEXT, LB_ADDSTRING, 0,
		   (LONG) (LPSTR) stdstrbuff);
    }
}

/* Handles browse classes dialog box */
INT_PTR CALLBACK BrowseClassesDlgProc(HWND hDlg, UINT msg, WPARAM wParam,
		      LPARAM lParam)
{
    INT i;
    static Class currClass;
    Class theClass;
    Inst theInst;
    Name theMember;
    WORD NotifyCode, wId;
    HBITMAP hBitmap;
    RECT aRect, DlgRect;
    HBITMAP hBmp;
    BITMAP bm;
    DRAWITEMSTRUCT FAR *lpdis;
    LPMEASUREITEMSTRUCT lpmis;
    LPCOMPAREITEMSTRUCT lpcis;
    BOOL Selected = FALSE;
    static HBITMAP hCBm, hCSelBm, hIBm, hISelBm, hMBm, hMSelBm;
    CHAR string1[256];

    NotifyCode = HIWORD(wParam);
    wId = LOWORD(wParam);

    switch (msg) {
	case WM_INITDIALOG:
	    CenterDialogInParent(hDlg);
	    SetDialogFont(hDlg, DefaultFont());

	    SendDlgItemMessage(hDlg, LB_CLASS, LB_SETHORIZONTALEXTENT,
		       (WPARAM) 300, 0L);
	    SendDlgItemMessage(hDlg, LB_MEMBERS,
		       LB_SETHORIZONTALEXTENT, (WPARAM) 400,
		       0L);

	    /* Create list of classes and set current class */
	    for (i = CLASSMIN; i < classMax(); i++) {
		SendDlgItemMessage(hDlg, LB_CLASS, LB_ADDSTRING, 0,
			   (LPARAM) (LPSTR) i);
	    }
	    SendDlgItemMessage(hDlg, LB_CLASS, LB_SETCURSEL, 0, 0L);
	    currClass =
		(Class) SendDlgItemMessage(hDlg, LB_CLASS,
			       LB_GETITEMDATA,
			       (WPARAM)
			       SendDlgItemMessage(hDlg,
					  LB_CLASS,
					  LB_GETCURSEL,
					  0,
					  0L),
			       0L);
	    SetClass(hDlg, currClass);

	    /* Create Bitmaps */
	    hCBm = LoadBitmap(hThisInstance, "CLASSBMP");
	    MapBitmap(hCBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_WINDOW));
	    hCSelBm = LoadBitmap(hThisInstance, "CLASSBMP");
	    MapBitmap(hCSelBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_HIGHLIGHT));
	    hIBm = LoadBitmap(hThisInstance, "INSTANCEBMP");
	    MapBitmap(hIBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_WINDOW));
	    hISelBm = LoadBitmap(hThisInstance, "INSTANCEBMP");
	    MapBitmap(hISelBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_HIGHLIGHT));
	    hMBm = LoadBitmap(hThisInstance, "MEMBERBMP");
	    MapBitmap(hMBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_WINDOW));
	    hMSelBm = LoadBitmap(hThisInstance, "MEMBERBMP");
	    MapBitmap(hMSelBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_HIGHLIGHT));
	    return (INT_PTR)TRUE;

	case WM_DESTROY:
	    /* Destroy Bitmaps */
	    DeleteObject(hCBm);
	    DeleteObject(hCSelBm);
	    DeleteObject(hIBm);
	    DeleteObject(hISelBm);
	    DeleteObject(hMBm);
	    DeleteObject(hMSelBm);
	    break;

	case WM_CTLCOLORBTN:
	case WM_CTLCOLORDLG:
	case WM_CTLCOLOREDIT:
	case WM_CTLCOLORLISTBOX:
	case WM_CTLCOLORMSGBOX:
	case WM_CTLCOLORSCROLLBAR:
	case WM_CTLCOLORSTATIC:
	    break;

	case WM_PAINT: {
		HDC hDC;
		PAINTSTRUCT Ps;

		BeginPaint(hDlg, &Ps);
		hDC = Ps.hdc;

		/* Paint classes Bitmap */
		GetWindowRect(hDlg, &DlgRect);
		GetWindowRect(GetDlgItem(hDlg, ID_PLACEBITMAP),
			  &aRect);

		hBitmap =
		    LoadMappedBitmap(hThisInstance,
			     "CLASSESDLGBMP");
		DrawBitmap(hDC, hBitmap,
		       aRect.left - DlgRect.left -
		       GetSystemMetrics(SM_CXDLGFRAME),
		       aRect.top - DlgRect.top -
		       GetSystemMetrics(SM_CYDLGFRAME) -
		       GetSystemMetrics(SM_CYCAPTION));
		DeleteObject(hBitmap);
		EndPaint(hDlg, &Ps);
	    }
	    break;

	case WM_COMPAREITEM: {

		lpcis = (COMPAREITEMSTRUCT FAR *) lParam;

		switch (wParam) {
		    case LB_CLASS:
			return strcmp(textToStr
				  (cclass(lpcis->itemData1).
				   text),
				  textToStr(cclass
				    (lpcis->itemData2).
				    text));

		    case LB_INSTANCES:
			if (nonNull
			    (inst(lpcis->itemData1).specifics)) {
			    printContext(stdstr,
				     inst(lpcis->
				      itemData1).
				     specifics);
			    fprintf(stdstr, " => ");
			}
			printPred(stdstr,
			      inst(lpcis->itemData1).head);
			fprintf(stdstr, "\n");
			strcpy(string1, stdstrbuff);
			if (nonNull
			    (inst(lpcis->itemData2).specifics)) {
			    printContext(stdstr,
				     inst(lpcis->
				      itemData2).
				     specifics);
			    fprintf(stdstr, " => ");
			}
			printPred(stdstr,
			      inst(lpcis->itemData2).head);
			fprintf(stdstr, "\n");
			return strcmp(string1, stdstrbuff);

		    case LB_MEMBERS:
			printExp(stdstr, lpcis->itemData1);
			fprintf(stdstr, "\n");
			strcpy(string1, stdstrbuff);
			printExp(stdstr, lpcis->itemData2);
			fprintf(stdstr, "\n");
			return strcmp(string1, stdstrbuff);

		}
	    }
	    break;

	case WM_MEASUREITEM:

	    lpdis = (DRAWITEMSTRUCT FAR *) lParam;

	    if (lpdis->CtlID == LB_CLASS ||
		lpdis->CtlID == LB_INSTANCES ||
		lpdis->CtlID == LB_MEMBERS) {

		lpmis = (LPMEASUREITEMSTRUCT) lParam;

		/* Set the height of the list box items to Bitmap height */
		hBmp = LoadBitmap(hThisInstance, "CLASSBMP");
		GetObject(hBmp, sizeof(BITMAP), &bm);
		DeleteObject(hBmp);
		lpmis->itemHeight = bm.bmHeight + 1;
		lpmis->itemWidth = 50000;

		return (INT_PTR)TRUE;
	    }
	    break;

	case WM_DRAWITEM:

	    lpdis = (DRAWITEMSTRUCT FAR *) lParam;

	    if (lpdis->CtlID == LB_CLASS ||
		lpdis->CtlID == LB_INSTANCES ||
		lpdis->CtlID == LB_MEMBERS) {

		if (lpdis->itemID == (UINT) - 1) {
		    return (INT_PTR)TRUE;
		}

		switch (lpdis->itemAction) {
		    case ODA_DRAWENTIRE:
		    case ODA_SELECT:
		    case ODA_FOCUS:
			if ((lpdis->
			     itemState & ODS_SELECTED)
			    /*&& (lpdis->itemState & ODS_FOCUS) */
			    ) {
			    SetBkColor(lpdis->hDC,
				   GetSysColor
				   (COLOR_HIGHLIGHT));
			    SetTextColor(lpdis->hDC,
				     GetSysColor
				     (COLOR_HIGHLIGHTTEXT));
			    Selected = TRUE;
			} else {
			    SetBkColor(lpdis->hDC,
				   GetSysColor
				   (COLOR_WINDOW));
			    SetTextColor(lpdis->hDC,
				     GetSysColor
				     (COLOR_WINDOWTEXT));
			}
			break;
		    default:
			return (INT_PTR)FALSE;
		}

		switch (lpdis->CtlID) {
		    case LB_CLASS:
			theClass =
			    (Class) SendDlgItemMessage(hDlg,
					   lpdis->
					   CtlID,
					   LB_GETITEMDATA,
					   lpdis->
					   itemID,
					   0);
			printPred(stdstr, cclass(theClass).head);
			fprintf(stdstr, "   -- in %s\n",
			    textToStr(module
				  (cclass(theClass).mod).
				  text));
			ExtTextOut(lpdis->hDC,
			       lpdis->rcItem.left + 21,
			       lpdis->rcItem.top, ETO_OPAQUE,
			       &(lpdis->rcItem), stdstrbuff,
			       strlen(stdstrbuff), NULL);
			hBmp = Selected ? hCSelBm : hCBm;

			break;

		    case LB_INSTANCES:
			theInst =
			    (Inst) SendDlgItemMessage(hDlg,
					  lpdis->
					  CtlID,
					  LB_GETITEMDATA,
					  lpdis->
					  itemID,
					  0);
			if (nonNull(inst(theInst).specifics)) {
			    printContext(stdstr,
				     inst(theInst).
				     specifics);
			    fprintf(stdstr, " => ");
			}
			printPred(stdstr, inst(theInst).head);
			fprintf(stdstr, "   -- in %s \n",
			    textToStr(module
				  (moduleOfScript
				   (scriptThisInst
				    (theInst))).text));
			ExtTextOut(lpdis->hDC,
			       lpdis->rcItem.left + 21,
			       lpdis->rcItem.top, ETO_OPAQUE,
			       &(lpdis->rcItem), stdstrbuff,
			       strlen(stdstrbuff), NULL);
			hBmp = Selected ? hISelBm : hIBm;
			break;

		    case LB_MEMBERS:
			theMember =
			    (Name) SendDlgItemMessage(hDlg,
					  lpdis->
					  CtlID,
					  LB_GETITEMDATA,
					  lpdis->
					  itemID,
					  0);
			printExp(stdstr, theMember);
			fprintf(stdstr, " :: ");
			printType(stdstr, name(theMember).type);
			fprintf(stdstr, "\n");
			ExtTextOut(lpdis->hDC,
			       lpdis->rcItem.left + 21,
			       lpdis->rcItem.top, ETO_OPAQUE,
			       &(lpdis->rcItem), stdstrbuff,
			       strlen(stdstrbuff), NULL);
			hBmp = Selected ? hMSelBm : hMBm;
			break;
		}
		DrawBitmap(lpdis->hDC, hBmp,
		       (lpdis->rcItem.left) + 4,
		       lpdis->rcItem.top);

		/* If selected draw rectangle */
		if ((lpdis->itemState & ODS_SELECTED)
		    && (lpdis->itemState & ODS_FOCUS)) {
		    DrawFocusRect(lpdis->hDC,
			      &(lpdis->rcItem));
		}

		return (INT_PTR)TRUE;
	    }

	case WM_COMMAND:
	    switch (wId) {
		case LB_CLASS:
		    switch (NotifyCode) {
			case LBN_SELCHANGE:	/* select a new class */
			    currClass =
				(Class) SendDlgItemMessage(hDlg,
					       LB_CLASS,
					       LB_GETITEMDATA,
					       SendDlgItemMessage
					       (hDlg,
						LB_CLASS,
						LB_GETCURSEL,
						0, 0L),
					       0L);
			    SetClass(hDlg, currClass);
			    break;

			case LBN_DBLCLK: {
				/* Open in text editor script file with class definition */
				currClass =
				    (Class)
				    SendDlgItemMessage(hDlg,
					       LB_CLASS,
					       LB_GETITEMDATA,
					       SendDlgItemMessage
					       (hDlg,
						LB_CLASS,
						LB_GETCURSEL,
						0, 0L),
					       0L);

				currClass =
				    (Class)
				    SendDlgItemMessage(hDlg,
					       LB_CLASS,
					       LB_GETITEMDATA,
					       SendDlgItemMessage
					       (hDlg,
						LB_CLASS,
						LB_GETCURSEL,
						0, 0L),
					       0L);
				setLastEdit(getScriptName
					(scriptThisClass
					 (currClass)),
					cclass(currClass).
					line);
				runEditor();

			    }
			    break;
		    }
		    break;

		case LB_MEMBERS:
		case LB_INSTANCES:
		    switch (NotifyCode) {	/* Open in text editor script file with instance definition */
			case LBN_DBLCLK: {
				Inst currInst;

				currInst =
				    (Inst)
				    SendDlgItemMessage(hDlg,
					       LB_INSTANCES,
					       LB_GETITEMDATA,
					       SendDlgItemMessage
					       (hDlg,
						LB_INSTANCES,
						LB_GETCURSEL,
						0, 0L),
					       0L);

				/* Find instance module */
				setLastEdit(getScriptName
					(scriptThisInst
					 (currInst)),
					inst(currInst).line);
				runEditor();

			    }
			    break;
		    }
		    break;

		case ID_HIERARCHY:	/* Draw classes hierarchy */
		    DrawClassesHierarchy();
		    break;

		case ID_EDITCLASS:	/* Pushed on Edit class button */
		    if (SendDlgItemMessage
			(hDlg, LB_CLASS, LB_GETCURSEL, 0,
			 0L) != LB_ERR)
			DlgSendMessage(hDlg, WM_COMMAND, LB_CLASS,
				   MAKELONG(0, LBN_DBLCLK));
		    break;

		case ID_EDITINSTANCE:	/* Pushed on Edit instance button */
		    if (SendDlgItemMessage
			(hDlg, LB_INSTANCES, LB_GETCURSEL, 0,
			 0L) != LB_ERR)
			DlgSendMessage(hDlg, WM_COMMAND,
				   LB_INSTANCES, MAKELONG(0,
					      LBN_DBLCLK));
		    break;

		case IDCANCEL:	/* Close dialog */
		    EndDialog(hDlg, FALSE);
		    return (INT_PTR)TRUE;
		case IDOK:
		    EndDialog(hDlg, TRUE);
		    return (INT_PTR)TRUE;

		default:
		    return (INT_PTR)TRUE;
	    }
    }
    return (INT_PTR)FALSE;
}

/* When the name  selected changes to currName gets its type and definition */
static local VOID SetName(HWND hDlg, UINT currName, List names)
{
    Name nm = nth(currName, names);

    if (nonNull(name(nm).type))
	printType(stdstr, name(nm).type);
    else
	fprintf(stdstr, "<Unknown type>");
    fprintf(stdstr, "\n");

    SendDlgItemMessage(hDlg, LB_NAMESTYPE, LB_RESETCONTENT, 0, 0L);
    SendDlgItemMessage(hDlg, LB_NAMESTYPE, LB_ADDSTRING, 0,
	       (LONG) (LPSTR) stdstrbuff);

    if (isCfun(nm))
	fprintf(stdstr, "Data constructor");
    else if (isMfun(nm))
	fprintf(stdstr, "Class member");
    else if (isSfun(nm))
	fprintf(stdstr, "Selector function");
    else if (name(nm).primDef)
	fprintf(stdstr, "Primitive");
    fprintf(stdstr, "\n");

    SendDlgItemMessage(hDlg, LB_NAMESNOTES, LB_RESETCONTENT, 0, 0L);
    SendDlgItemMessage(hDlg, LB_NAMESNOTES, LB_ADDSTRING, 0,
	       (LONG) (LPSTR) stdstrbuff);
}

/* Handles browse names dialog box */
INT_PTR CALLBACK BrowseNamesDlgProc(HWND hDlg, UINT msg, WPARAM wParam,
		    LPARAM lParam)
{
    static List namesList = NIL;
    List names = NIL;
    struct strName nm;
    Name n;
    UINT theName;
    WORD NotifyCode, wId;
    RECT aRect, DlgRect;
    HBITMAP hBitmap;
    HBITMAP hBmp;
    BITMAP bm;
    static HBITMAP hPBm, hPSelBm, hDBm, hDSelBm, hMBm, hMSelBm, hNBm,
	hNSelBm, hSBm, hSSelBm;
    CHAR Buffer[300];
    DRAWITEMSTRUCT FAR *lpdis;
    LPMEASUREITEMSTRUCT lpmis;
    BOOL Selected = FALSE;

    NotifyCode = HIWORD(wParam);
    wId = LOWORD(wParam);

    switch (msg) {

	case WM_INITDIALOG:
	    CenterDialogInParent(hDlg);
	    SetDialogFont(hDlg, DefaultFont());
	    namesList = addNamesMatching((String) 0, NIL);

	    /* Clear the redraw flag */
	    SendDlgItemMessage(hDlg, LB_NAMES, WM_SETREDRAW, FALSE,
		       0L);

	    for (names = namesList; nonNull(names); names = tl(names)) {
		if (nonNull(names)) {
		    nm = name(hd(names));
		    fprintf(stdstr, "%s   -- in %s\n",
			textToStr(nm.text),
			textToStr(module(nm.mod).text));
		    SendDlgItemMessage(hDlg, LB_NAMES,
			       LB_ADDSTRING, 0,
			       (LONG) (LPSTR)
			       stdstrbuff);
		    SendDlgItemMessage(hDlg, LB_NAMES,
			       LB_SETCURSEL, 0, 0L);
		}
	    }

	    /* Set the redraw flag and force repaint. */
	    SendDlgItemMessage(hDlg, LB_NAMES, WM_SETREDRAW, TRUE, 0L);
	    InvalidateRect(GetDlgItem(hDlg, LB_NAMES), NULL, TRUE);

	    theName = 0;
	    SetName(hDlg, theName, namesList);

	    hPBm = LoadBitmap(hThisInstance, "PRIMBMP");
	    MapBitmap(hPBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_WINDOW));
	    hPSelBm = LoadBitmap(hThisInstance, "PRIMBMP");
	    MapBitmap(hPSelBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_HIGHLIGHT));
	    hDBm = LoadBitmap(hThisInstance, "DATACONSBMP");
	    MapBitmap(hDBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_WINDOW));
	    hDSelBm = LoadBitmap(hThisInstance, "DATACONSBMP");
	    MapBitmap(hDSelBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_HIGHLIGHT));
	    hMBm = LoadBitmap(hThisInstance, "MEMBERBMP");
	    MapBitmap(hMBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_WINDOW));
	    hMSelBm = LoadBitmap(hThisInstance, "MEMBERBMP");
	    MapBitmap(hMSelBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_HIGHLIGHT));
	    hNBm = LoadBitmap(hThisInstance, "NAMEBMP");
	    MapBitmap(hNBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_WINDOW));
	    hNSelBm = LoadBitmap(hThisInstance, "NAMEBMP");
	    MapBitmap(hNSelBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_HIGHLIGHT));
	    hSBm = LoadBitmap(hThisInstance, "SELECTORBMP");
	    MapBitmap(hSBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_WINDOW));
	    hSSelBm = LoadBitmap(hThisInstance, "SELECTORBMP");
	    MapBitmap(hSSelBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_HIGHLIGHT));

	    /* set focus to search box (must return FALSE) */
	    SetFocus(GetDlgItem(hDlg, IDC_SEARCHNAME));
	    return (INT_PTR)FALSE;

	case WM_DESTROY:
	    DeleteObject(hPBm);
	    DeleteObject(hPSelBm);
	    DeleteObject(hDBm);
	    DeleteObject(hDSelBm);
	    DeleteObject(hMBm);
	    DeleteObject(hMSelBm);
	    DeleteObject(hNBm);
	    DeleteObject(hNSelBm);
	    DeleteObject(hSBm);
	    DeleteObject(hSSelBm);
	    break;

	case WM_CTLCOLORBTN:
	case WM_CTLCOLORDLG:
	case WM_CTLCOLOREDIT:
	case WM_CTLCOLORLISTBOX:
	case WM_CTLCOLORMSGBOX:
	case WM_CTLCOLORSCROLLBAR:
	case WM_CTLCOLORSTATIC:
	    break;

	case WM_MEASUREITEM:

	    lpdis = (DRAWITEMSTRUCT FAR *) lParam;

	    if (lpdis->CtlID == LB_NAMES) {

		lpmis = (LPMEASUREITEMSTRUCT) lParam;

		/* Set the height of the list box items to Bitmap height */
		hBmp = LoadBitmap(hThisInstance, "PRIMBMP");
		GetObject(hBmp, sizeof(BITMAP), &bm);
		DeleteObject(hBmp);

		lpmis->itemHeight = bm.bmHeight + 1;

		return (INT_PTR)TRUE;
	    }
	    break;

	case WM_DRAWITEM:

	    lpdis = (DRAWITEMSTRUCT FAR *) lParam;

	    if (lpdis->CtlID == LB_NAMES) {

		if (lpdis->itemID == (UINT) - 1) {
		    return (INT_PTR)TRUE;
		}

		switch (lpdis->itemAction) {
		    case ODA_DRAWENTIRE:
		    case ODA_SELECT:
		    case ODA_FOCUS:
			if ((lpdis->
			     itemState & ODS_SELECTED)
			    /*&& (lpdis->itemState & ODS_FOCUS) */
			    ) {
			    SetBkColor(lpdis->hDC,
				   GetSysColor
				   (COLOR_HIGHLIGHT));
			    SetTextColor(lpdis->hDC,
				     GetSysColor
				     (COLOR_HIGHLIGHTTEXT));
			    Selected = TRUE;
			} else {
			    SetBkColor(lpdis->hDC,
				   GetSysColor
				   (COLOR_WINDOW));
			    SetTextColor(lpdis->hDC,
				     GetSysColor
				     (COLOR_WINDOWTEXT));
			}
			break;

		    default:
			return (INT_PTR)FALSE;
		}

		SendDlgItemMessage(hDlg, lpdis->CtlID, LB_GETTEXT,
			   lpdis->itemID, (LPARAM) Buffer);
		ExtTextOut(lpdis->hDC, lpdis->rcItem.left + 21,
		       lpdis->rcItem.top, ETO_OPAQUE,
		       &(lpdis->rcItem), Buffer,
		       strlen(Buffer), NULL);

		n = nth(lpdis->itemID, namesList);

		if (isCfun(n))
		    hBmp = Selected ? hDSelBm : hDBm;
		else if (isMfun(n))
		    hBmp = Selected ? hMSelBm : hMBm;
		else if (isSfun(n))
		    hBmp = Selected ? hSSelBm : hSBm;
		else if (name(n).primDef)
		    hBmp = Selected ? hPSelBm : hPBm;
		else
		    hBmp = Selected ? hNSelBm : hNBm;

		DrawBitmap(lpdis->hDC, hBmp,
		       (lpdis->rcItem.left) + 4,
		       lpdis->rcItem.top);
		/* If selected draw rectangle */
		if ((lpdis->itemState & ODS_SELECTED)
		    && (lpdis->itemState & ODS_FOCUS)) {
		    DrawFocusRect(lpdis->hDC,
			      &(lpdis->rcItem));
		}

		return (INT_PTR)TRUE;
	    }

	case WM_PAINT: {
		HDC hDC;
		PAINTSTRUCT Ps;

		BeginPaint(hDlg, &Ps);
		hDC = Ps.hdc;

		/* Paint classes Bitmap */
		GetWindowRect(hDlg, &DlgRect);
		GetWindowRect(GetDlgItem(hDlg, ID_PLACEBITMAP),
			  &aRect);

		hBitmap =
		    LoadMappedBitmap(hThisInstance,
			     "NAMESDLGBMP");
		DrawBitmap(hDC, hBitmap,
		       aRect.left - DlgRect.left -
		       GetSystemMetrics(SM_CXDLGFRAME),
		       aRect.top - DlgRect.top -
		       GetSystemMetrics(SM_CYDLGFRAME) -
		       GetSystemMetrics(SM_CYCAPTION));
		DeleteObject(hBitmap);
		EndPaint(hDlg, &Ps);
	    }
	    break;

	case WM_COMMAND:
	    switch (wId) {
		case LB_NAMES:
		    switch (NotifyCode) {
			case LBN_SELCHANGE:
			    /* Select a new name */
			    theName =
				(UINT) SendDlgItemMessage(hDlg,
					      LB_NAMES,
					      LB_GETCURSEL,
					      0, 0L);
			    SetName(hDlg, theName, namesList);
			    break;

			case LBN_DBLCLK: {
				/* Open in text editor script file with name definition */
				Name n;

				/* Get the selected name */
				theName =
				    (UINT)
				    SendDlgItemMessage(hDlg,
					       LB_NAMES,
					       LB_GETCURSEL,
					       0, 0L);
				n = nth(theName, namesList);

				if (!name(n).primDef) {
				    setLastEdit(getScriptName
					    (scriptThisName
					     (n)),
					    name(n).line);
				    runEditor();
				} else {
				    InfoBox("Primitive function:\nNo definition available.");
				}
			    }
			    break;
		    }
		    break;

		case IDC_SEARCHNAME:	/* Search a name */
		    switch (HIBYTE(NotifyCode)) {
			case HIBYTE(EN_CHANGE): {
				CHAR Buffer[300];

				/* Get edit control contents */
				SendDlgItemMessage(hDlg,
					   IDC_SEARCHNAME,
					   WM_GETTEXT, 300,
					   (LPARAM) ((LPSTR) Buffer));
				/* Search in names list box */
				SendDlgItemMessage(hDlg, LB_NAMES,
					   LB_SELECTSTRING,
					   0,
					   (LPARAM) ((LPSTR) Buffer));
				/* Update window contents */
				DlgSendMessage(hDlg, WM_COMMAND,
					   LB_NAMES,
					   MAKELONG(0,
					    LBN_SELCHANGE));
			    }
			    break;
		    }
		    break;

		case ID_EDITNAME:	/* Pushed on Edit name button */
		    if (SendDlgItemMessage
			(hDlg, LB_NAMES, LB_GETCURSEL, 0,
			 0L) != LB_ERR)
			DlgSendMessage(hDlg, WM_COMMAND, LB_NAMES,
				   MAKELONG(0, LBN_DBLCLK));
		    break;

		case IDCANCEL:	/* Close dialog */
		    EndDialog(hDlg, FALSE);
		    return (INT_PTR)TRUE;
		case IDOK:
		    EndDialog(hDlg, TRUE);
		    return (INT_PTR)TRUE;

		default:
		    return (INT_PTR)TRUE;
	    }
    }
    return (INT_PTR)FALSE;
}

static Int numCfuns;
static Int numSfuns;

/* A new Tycon was selected */
static local VOID SetTycon(HWND hDlg, UINT currTycon, List tycons)
{
    Tycon tc;
    Int j;
    Type t;
    Inst in;

    tc = nth(currTycon, tycons);
    numCfuns = 0;
    numSfuns = 0;

    t = tc;
    for (j = 0; j < tycon(tc).arity; ++j)
	t = ap(t, mkOffset(j));

    switch (tycon(tc).what) {
	case SYNONYM:
	    fprintf(stdstr, "type ");
	    printType(stdstr, t);
	    fprintf(stdstr, " = ");
	    printType(stdstr, tycon(tc).defn);
	    fprintf(stdstr, "\n");

	    SendDlgItemMessage(hDlg, LB_CONS, LB_RESETCONTENT, 0, 0L);
	    SendDlgItemMessage(hDlg, LB_DEF, LB_RESETCONTENT, 0, 0L);
	    SendDlgItemMessage(hDlg, LB_DEF, LB_ADDSTRING, 0,
		       (LONG) (LPSTR) stdstrbuff);
	    break;

	case NEWTYPE:
	case DATATYPE: {
		List cs = tycon(tc).defn;

		if (tycon(tc).what == DATATYPE)
		    fprintf(stdstr, "data ");
		else
		    fprintf(stdstr, "newtype ");
		printType(stdstr, t);
		fprintf(stdstr, "\n");
		SendDlgItemMessage(hDlg, LB_DEF, LB_RESETCONTENT,
			   0, 0L);
		SendDlgItemMessage(hDlg, LB_DEF, LB_ADDSTRING, 0,
			   (LONG) (LPSTR) stdstrbuff);

		SendDlgItemMessage(hDlg, LB_CONS, LB_RESETCONTENT,
			   0, 0L);

		/* Clear the redraw flag */
		SendDlgItemMessage(hDlg, LB_CONS, WM_SETREDRAW,
			   FALSE, 0L);

		for (; nonNull(cs); cs = tl(cs)) {
		    printExp(stdstr, hd(cs));
		    fprintf(stdstr, " :: ");
		    printType(stdstr, name(hd(cs)).type);
		    fprintf(stdstr, "\n");
		    SendDlgItemMessage(hDlg, LB_CONS,
			       LB_ADDSTRING, 0,
			       (LONG) (LPSTR)
			       stdstrbuff);
		    SendDlgItemMessage(hDlg, LB_CONS,
			       LB_SETCURSEL, 0, 0L);
		    if (isCfun(hd(cs)))
			numCfuns++;
		    else
			numSfuns++;
		}

		/* Set the redraw flag and force repaint. */
		SendDlgItemMessage(hDlg, LB_CONS, WM_SETREDRAW,
			   TRUE, 0L);
		InvalidateRect(GetDlgItem(hDlg, LB_CONS), NULL,
			   TRUE);

		break;
	    }
	case RESTRICTSYN:
	    fprintf(stdstr, "type");
	    printType(stdstr, t);
	    fprintf(stdstr, " = <restricted>\n");

	    SendDlgItemMessage(hDlg, LB_CONS, LB_RESETCONTENT, 0, 0L);
	    SendDlgItemMessage(hDlg, LB_DEF, LB_RESETCONTENT, 0, 0L);
	    SendDlgItemMessage(hDlg, LB_DEF, LB_ADDSTRING, 0,
		       (LONG) (LPSTR) stdstrbuff);
	    break;
    }

    /* Set instances */
    SendDlgItemMessage(hDlg, LB_TYCONSINST, LB_RESETCONTENT, 0, 0L);
    /* Clear the redraw flag */
    SendDlgItemMessage(hDlg, LB_TYCONSINST, WM_SETREDRAW, FALSE, 0L);

    if (nonNull(in = findFirstInst(tc))) {
	do {
	    SendDlgItemMessage(hDlg, LB_TYCONSINST,
		       LB_ADDSTRING, 0,
		       (LONG) (LPSTR) in);
	    SendDlgItemMessage(hDlg, LB_TYCONSINST,
		       LB_SETCURSEL, 0, 0L);
	    in = findNextInst(tc, in);
	} while (nonNull(in));
    }
    /* Set the redraw flag and force repaint. */
    SendDlgItemMessage(hDlg, LB_TYCONSINST, WM_SETREDRAW, TRUE, 0L);
    InvalidateRect(GetDlgItem(hDlg, LB_TYCONSINST), NULL, TRUE);

}

/* Handles browse Tycons dialog box */
INT_PTR CALLBACK BrowseTyconsDlgProc(HWND hDlg, UINT msg, WPARAM wParam,
		     LPARAM lParam)
{
    static List tyconList = NIL;
    List tycons = NIL;
    Tycon tc;
    UINT theTycon;
    WORD NotifyCode, wId;
    RECT aRect, DlgRect;
    HBITMAP hBitmap;
    HBITMAP hBmp;
    BITMAP bm;
    static HBITMAP hTCBm, hTCSelBm, hDBm, hDSelBm, hTSBm, hTSSelBm,
	hNTBm, hNTSelBm, hSBm, hSSelBm, hIBm, hISelBm;
    CHAR Buffer[300];
    DRAWITEMSTRUCT FAR *lpdis;
    LPMEASUREITEMSTRUCT lpmis;
    BOOL Selected = FALSE;
    Inst theInst;

    NotifyCode = HIWORD(wParam);
    wId = LOWORD(wParam);

    switch (msg) {

	case WM_INITDIALOG:
	    CenterDialogInParent(hDlg);
	    SetDialogFont(hDlg, DefaultFont());
	    tyconList = addTyconsMatching((String) 0, NIL);

	    /* Clear the redraw flag */
	    SendDlgItemMessage(hDlg, LB_TYCONS, WM_SETREDRAW, FALSE,
		       0L);

	    for (tycons = tyconList; nonNull(tycons);
		 tycons = tl(tycons)) {
		if (nonNull(tycons)) {
		    tc = hd(tycons);
		    fprintf(stdstr, "%s   -- in %s\n",
			textToStr(tycon(tc).text),
			textToStr(module(tycon(tc).mod).
			      text));
		    SendDlgItemMessage(hDlg, LB_TYCONS,
			       LB_ADDSTRING, 0,
			       (LONG) (LPSTR)
			       stdstrbuff);
		    SendDlgItemMessage(hDlg, LB_TYCONS,
			       LB_SETCURSEL, 0, 0L);
		}
		/* Set the redraw flag and force repaint. */
		SendDlgItemMessage(hDlg, LB_TYCONS, WM_SETREDRAW,
			   TRUE, 0L);
		InvalidateRect(GetDlgItem(hDlg, LB_TYCONS), NULL,
			   TRUE);
	    }

	    theTycon = 0;
	    SetTycon(hDlg, theTycon, tyconList);

	    hTCBm = LoadBitmap(hThisInstance, "TYPECONSBMP");
	    MapBitmap(hTCBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_WINDOW));
	    hTCSelBm = LoadBitmap(hThisInstance, "TYPECONSBMP");
	    MapBitmap(hTCSelBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_HIGHLIGHT));
	    hDBm = LoadBitmap(hThisInstance, "DATACONSBMP");
	    MapBitmap(hDBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_WINDOW));
	    hDSelBm = LoadBitmap(hThisInstance, "DATACONSBMP");
	    MapBitmap(hDSelBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_HIGHLIGHT));
	    hTSBm = LoadBitmap(hThisInstance, "TYPESINBMP");
	    MapBitmap(hTSBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_WINDOW));
	    hTSSelBm = LoadBitmap(hThisInstance, "TYPESINBMP");
	    MapBitmap(hTSSelBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_HIGHLIGHT));
	    hNTBm = LoadBitmap(hThisInstance, "NEWTYPEBMP");
	    MapBitmap(hNTBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_WINDOW));
	    hNTSelBm = LoadBitmap(hThisInstance, "NEWTYPEBMP");
	    MapBitmap(hNTSelBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_HIGHLIGHT));
	    hSBm = LoadBitmap(hThisInstance, "SELECTORBMP");
	    MapBitmap(hSBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_WINDOW));
	    hSSelBm = LoadBitmap(hThisInstance, "SELECTORBMP");
	    MapBitmap(hSSelBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_HIGHLIGHT));
	    hIBm = LoadBitmap(hThisInstance, "INSTANCEBMP");
	    MapBitmap(hIBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_WINDOW));
	    hISelBm = LoadBitmap(hThisInstance, "INSTANCEBMP");
	    MapBitmap(hISelBm, RGB(0, 128, 128),
		  GetSysColor(COLOR_HIGHLIGHT));

	    /* set focus to search box (must return FALSE) */
	    SetFocus(GetDlgItem(hDlg, IDC_SEARCHTYCON));
	    return (INT_PTR)FALSE;

	case WM_DESTROY:
	    DeleteObject(hTCBm);
	    DeleteObject(hTCSelBm);
	    DeleteObject(hDBm);
	    DeleteObject(hDSelBm);
	    DeleteObject(hTSBm);
	    DeleteObject(hTSSelBm);
	    DeleteObject(hNTBm);
	    DeleteObject(hNTSelBm);
	    DeleteObject(hSBm);
	    DeleteObject(hSSelBm);
	    DeleteObject(hIBm);
	    DeleteObject(hISelBm);

	    break;

	case WM_CTLCOLORBTN:
	case WM_CTLCOLORDLG:
	case WM_CTLCOLOREDIT:
	case WM_CTLCOLORLISTBOX:
	case WM_CTLCOLORMSGBOX:
	case WM_CTLCOLORSCROLLBAR:
	case WM_CTLCOLORSTATIC:
	    break;

	case WM_MEASUREITEM:

	    lpdis = (DRAWITEMSTRUCT FAR *) lParam;

	    if (lpdis->CtlID == LB_TYCONS ||
		lpdis->CtlID == LB_CONS ||
		lpdis->CtlID == LB_TYCONSINST) {

		lpmis = (LPMEASUREITEMSTRUCT) lParam;

		/* Set the height of the list box items to Bitmap height */
		hBmp = LoadBitmap(hThisInstance, "CLASSBMP");
		GetObject(hBmp, sizeof(BITMAP), &bm);
		DeleteObject(hBmp);

		lpmis->itemHeight = bm.bmHeight + 1;

		return (INT_PTR)TRUE;
	    }
	    break;

	case WM_DRAWITEM:

	    lpdis = (DRAWITEMSTRUCT FAR *) lParam;

	    if (lpdis->CtlID == LB_TYCONS ||
		lpdis->CtlID == LB_CONS ||
		lpdis->CtlID == LB_TYCONSINST) {

		if (lpdis->itemID == (UINT) - 1) {
		    return (INT_PTR)TRUE;
		}

		switch (lpdis->itemAction) {
		case ODA_DRAWENTIRE:
		case ODA_SELECT:
		case ODA_FOCUS:
		    if ((lpdis->
			 itemState & ODS_SELECTED)
			/*&& (lpdis->itemState & ODS_FOCUS) */
			) {
			SetBkColor(lpdis->hDC,
			       GetSysColor
			       (COLOR_HIGHLIGHT));
			SetTextColor(lpdis->hDC,
				 GetSysColor
				 (COLOR_HIGHLIGHTTEXT));
			Selected = TRUE;
		    } else {
			SetBkColor(lpdis->hDC,
			       GetSysColor
			       (COLOR_WINDOW));
			SetTextColor(lpdis->hDC,
				 GetSysColor
				 (COLOR_WINDOWTEXT));
		    }
		    break;

		default:
		    return (INT_PTR)FALSE;
		}

		SendDlgItemMessage(hDlg, lpdis->CtlID, LB_GETTEXT,
			   lpdis->itemID, (LPARAM) Buffer);

		ExtTextOut(lpdis->hDC, lpdis->rcItem.left + 21,
		       lpdis->rcItem.top, ETO_OPAQUE,
		       &(lpdis->rcItem), Buffer,
		       strlen(Buffer), NULL);

		switch (lpdis->CtlID) {
		case LB_TYCONS:
		    theTycon = (UINT) lpdis->itemID;
		    tc = nth(theTycon, tyconList);

		    switch (tycon(tc).what) {
		    case RESTRICTSYN:
		    case SYNONYM:
			hBmp = Selected ? hTSSelBm : hTSBm;
			break;
		    case DATATYPE:
			hBmp = Selected ? hTCSelBm : hTCBm;
			break;
		    case NEWTYPE:
			hBmp = Selected ? hNTSelBm : hNTBm;
			break;
		    }
		    break;

		case LB_CONS:
		    if (lpdis->itemID >= (UINT) numCfuns)
			hBmp = Selected ? hSSelBm : hSBm;
		    else
			hBmp = Selected ? hDSelBm : hDBm;
		    break;
		case LB_TYCONSINST:
		    theInst =
			(Inst) SendDlgItemMessage(hDlg,
				      lpdis->
				      CtlID,
				      LB_GETITEMDATA,
				      lpdis->
				      itemID,
				      0);
		    if (nonNull(inst(theInst).specifics)) {
			printContext(stdstr,
				 inst(theInst).
				 specifics);
			fprintf(stdstr, " => ");
		    }
		    printPred(stdstr, inst(theInst).head);
		    fprintf(stdstr, "   -- in %s \n",
			textToStr(module
			      (moduleOfScript
			       (scriptThisInst
				(theInst))).text));
		    ExtTextOut(lpdis->hDC,
			   lpdis->rcItem.left + 21,
			   lpdis->rcItem.top, ETO_OPAQUE,
			   &(lpdis->rcItem), stdstrbuff,
			   strlen(stdstrbuff), NULL);

		    hBmp = Selected ? hISelBm : hIBm;
		    break;
		}

		DrawBitmap(lpdis->hDC, hBmp,
		       (lpdis->rcItem.left) + 4,
		       lpdis->rcItem.top);

		/* If selected draw rectangle */
		if ((lpdis->itemState & ODS_SELECTED)
		    && (lpdis->itemState & ODS_FOCUS)) {
		    DrawFocusRect(lpdis->hDC,
			      &(lpdis->rcItem));
		}

		return (INT_PTR)TRUE;
	    }

	case WM_PAINT: {
		HDC hDC;
		PAINTSTRUCT Ps;

		BeginPaint(hDlg, &Ps);
		hDC = Ps.hdc;

		/* Paint classes Bitmap */
		GetWindowRect(hDlg, &DlgRect);
		GetWindowRect(GetDlgItem(hDlg, ID_PLACEBITMAP),
			  &aRect);

		hBitmap =
		    LoadMappedBitmap(hThisInstance,
			     "TYCONSDLGBMP");
		DrawBitmap(hDC, hBitmap,
		       aRect.left - DlgRect.left -
		       GetSystemMetrics(SM_CXDLGFRAME),
		       aRect.top - DlgRect.top -
		       GetSystemMetrics(SM_CYDLGFRAME) -
		       GetSystemMetrics(SM_CYCAPTION));

		DeleteObject(hBitmap);
		EndPaint(hDlg, &Ps);
	    }
	    break;

	case WM_COMMAND:
	    switch (LOWORD(wId)) {
	    case LB_TYCONS:
		switch (NotifyCode) {
		case LBN_SELCHANGE: {
			/* A new tycon was selected */
			theTycon =
			    (UINT)
			    SendDlgItemMessage(hDlg,
				       LB_TYCONS,
				       LB_GETCURSEL,
				       0, 0L);
			SetTycon(hDlg, theTycon,
			     tyconList);
		    }
		    break;

		case LBN_DBLCLK: {
			/* Open in text editor script file with instance definition */
			INT TheTycon;
			Tycon tc;

			/* Get selected tycon */
			TheTycon =
			    (UINT)
			    SendDlgItemMessage(hDlg,
				       LB_TYCONS,
				       LB_GETCURSEL,
				       0, 0L);
			tc = nth(TheTycon, tyconList);

			if (isTycon(tc) && tycon(tc).line) {
			    setLastEdit(getScriptName
				    (scriptThisTycon
				     (tc)),
				    tycon(tc).
				    line);
			    runEditor();
			} else {
			    InfoBox("Primitive type:\nNo definition available.");
			}
		    }
		    break;
		}
		break;

	    case LB_TYCONSINST:
		switch (NotifyCode) {
		case LBN_DBLCLK: {
			Inst currInst;

			currInst =
			    (Inst)
			    SendDlgItemMessage(hDlg,
				       LB_TYCONSINST,
				       LB_GETITEMDATA,
				       SendDlgItemMessage
				       (hDlg,
					LB_TYCONSINST,
					LB_GETCURSEL,
					0, 0L),
				       0L);

			/* Find instance module */
			setLastEdit(getScriptName
				(scriptThisInst
				 (currInst)),
				inst(currInst).line);
			runEditor();
		    }
		    break;
		}
		break;

	    case IDC_SEARCHTYCON:	/* Search a name */
		switch (HIBYTE(wId)) {
		case HIBYTE(EN_CHANGE): {
			CHAR Buffer[300];

			/* Get edit control contents */
			SendDlgItemMessage(hDlg,
				   IDC_SEARCHTYCON,
				   WM_GETTEXT, 300,
				   (LPARAM) ((LPSTR) Buffer));

			/* Search in names list box */
			SendDlgItemMessage(hDlg, LB_TYCONS,
				   LB_SELECTSTRING,
				   0,
				   (LPARAM) ((LPSTR) Buffer));

			/* Update window contents */
			DlgSendMessage(hDlg, WM_COMMAND,
				   LB_TYCONS,
				   MAKELONG(0,
				    LBN_SELCHANGE));
		    }
		    break;
		}
		break;

	    case LB_CONS:
		switch (NotifyCode) {
		case LBN_DBLCLK: {

			/* Open in text editor script file with constructor definition */
			DlgSendMessage(hDlg, WM_COMMAND,
				   ID_EDITTYCON, 0L);
			break;
		    }
		}
		break;

	    case ID_EDITTYCON:	/* Pushed on Edit tycon button */
		if (SendDlgItemMessage
		    (hDlg, LB_TYCONS, LB_GETCURSEL, 0,
		     0L) != LB_ERR)
		    DlgSendMessage(hDlg, WM_COMMAND, LB_TYCONS,
			       MAKELONG(0, LBN_DBLCLK));
		break;

	    case ID_EDITTYCONSINST:
		if (SendDlgItemMessage
		    (hDlg, LB_TYCONSINST, LB_GETCURSEL, 0,
		     0L) != LB_ERR)
		    DlgSendMessage(hDlg, WM_COMMAND,
			       LB_TYCONSINST, MAKELONG(0,
					   LBN_DBLCLK));
		break;

	    case IDCANCEL:	/* Close dialog */
		EndDialog(hDlg, FALSE);
		return (INT_PTR)TRUE;
	    case IDOK:
		EndDialog(hDlg, TRUE);
		return (INT_PTR)TRUE;

	    default:
		return (INT_PTR)TRUE;
	    }
    }
    return (INT_PTR)FALSE;
}

/*-----------------------------------------------------------------------------
 * Class Hierarchy browser
 *
 * When the hierarchy browser is created, we call buildClassGraph to
 *  construct a table of class-position pairs.
 * The positions in the table can be adjusted using left button to drag nodes.
 * Edges (superclass relationships) are added in as the graph is being drawn.
 *----------------------------------------------------------------------------*/

static VOID local setClassBrowserSize Args((Void));
static VOID local doCreate_Classes Args((HWND));
static VOID local doDestroy_Classes Args((Void));
static Void local doMove_Classes Args((HWND, INT, INT));
static Void local doSize_Classes Args((HWND, INT, INT));
static Void local doPaint_Classes Args((HWND));
static Void local setOffset_Classes Args((HWND, INT, INT));
static Void local lButtonDown_Classes Args((HWND, INT, INT));
static Void local lButtonUp_Classes Args((HWND, INT, INT));
static Void local doMouseMove_Classes Args((HWND, INT, INT));
static Void local doGetMinMaxInfo_Classes Args((MINMAXINFO FAR *));

/* Layout controls */

#define VERTICAL_SEPARATION      35
#define HORIZONTAL_SEPARATION    55
#define INIT_COLUMN              10
#define INIT_ROW		 20
#define MAX_WIDTH  		600
#define MAX_HEIGHT 		500

/* structure used to draw class hierarchy */
typedef struct {
    RECT Pos;
    Class Class;
} HierarchyInfo;

typedef INT Node;
static HierarchyInfo *Nodes = NULL;	/* The list of nodes             */
static Node LastNode = 0;

static Node local findClassInNodes Args((Class));
static Bool local allocNodes Args((INT));
static Void local drawNode Args((HDC, Node));
static Void local drawClassRelations Args((HDC));

static Bool local allocNodes(INT n)
{				/* Get memory for nodes list */
    if (Nodes)
	free(Nodes);
    Nodes = calloc((ULONG) (sizeof(HierarchyInfo)), (ULONG) n);
    LastNode = 0;
    return (Nodes != NULL);
}

static Node local findClassInNodes(Class cls)
{
    Node n;

    for (n = 0; n < LastNode; n++) {
	if (Nodes[n].Class == cls) {
	    return n;
	}
    }
    return -1;
}

static Bool local isParentOf(Class parent, Class child)
{
    List supers;

    for (supers = cclass(child).supers; nonNull(supers);
	 supers = tl(supers)) {
	if (getHead(hd(supers)) == parent) {
	    return TRUE;
	}
    }
    return FALSE;
}

/* Add a class and all its children recursive */
/* returns the row for placing next node      */
static INT local addClassToGraph(HDC hDC, INT Column, INT startRow,
		 Class ThisClass)
{
    Node newNode = LastNode++;
    SIZE Size;
    INT row = startRow;

    /* Get size of class name on the screen */
    fprintf(stdstr, "%s\n", textToStr(cclass(ThisClass).text));
    GetTextExtentPoint(hDC, (LPSTR) stdstrbuff,
	       (INT) strlen(stdstrbuff), &Size);

    Nodes[newNode].Class = ThisClass;
    Nodes[newNode].Pos.left = Column;
    Nodes[newNode].Pos.top = startRow;
    Nodes[newNode].Pos.right = Nodes[newNode].Pos.left + Size.cx;
    Nodes[newNode].Pos.bottom = Nodes[newNode].Pos.top + Size.cy;

    /* Add subclasses of ThisClass */
    {
	Class cls;
	INT col = Nodes[newNode].Pos.right + HORIZONTAL_SEPARATION;
	INT child = 0;

	for (cls = CLASSMIN; cls < classMax(); cls++) {
	    if (-1 == findClassInNodes(cls)	/* Check for cycles in graph */
		&&isParentOf(ThisClass, cls)) {
		if (child++ > 0) {
		    row += VERTICAL_SEPARATION;
		}
		row = addClassToGraph(hDC, col, row, cls);
	    }
	}
    }
    /* Set to average position of children */
    {
	INT height = row - startRow;

	Nodes[newNode].Pos.top += height / 2;
	Nodes[newNode].Pos.bottom += height / 2;
    }
    return row;
}

static Void local buildClassGraph(HDC hDC)
{
    INT row = INIT_ROW;
    Class cls;

    for (cls = CLASSMIN; cls < classMax(); cls++) {
	if (cclass(cls).numSupers == 0) {
	    row = addClassToGraph(hDC, INIT_COLUMN, row,
			  cls) + VERTICAL_SEPARATION;
	}
    }
    /* Since Haskell has acyclic class dependencies, we should be done by now;
     * but it does no harm to make sure.
     */
    for (cls = CLASSMIN; cls < classMax(); cls++) {
	if (-1 == findClassInNodes(cls)) {	/* Not added yet */
	    row = addClassToGraph(hDC, INIT_COLUMN, row,
			  cls) + VERTICAL_SEPARATION;
	}
    }
}

static Void local drawClassRelations(HDC hDC)
{
    Class cls;

    for (cls = CLASSMIN; cls < classMax(); cls++) {
	List supers;

	for (supers = cclass(cls).supers; nonNull(supers);
	     supers = tl(supers)) {
	    Class parent = getHead(hd(supers));

	    if (isClass(parent)) {
		if (parent == cls) {	/* child of itself - draw an arc */
		    Class source =
			findClassInNodes(cls);
		    Arc(hDC,
			Nodes[source].Pos.right - 5,
			Nodes[source].Pos.bottom - 5,
			Nodes[source].Pos.right + 15,
			Nodes[source].Pos.bottom + 20,
			Nodes[source].Pos.right - 5,
			Nodes[source].Pos.bottom - 5,
			Nodes[source].Pos.right - 4,
			Nodes[source].Pos.bottom - 4);
		} else {	/* Join the two classes with a line */
		    Class source =
			findClassInNodes(parent);
		    Class target =
			findClassInNodes(cls);

		    INT sx = Nodes[source].Pos.right +
			4;
		    INT sy = Nodes[source].Pos.top +
			(Nodes[source].Pos.bottom -
			 Nodes[source].Pos.top) /
			2;
		    INT tx = Nodes[target].Pos.left -
			4;
		    INT ty = Nodes[target].Pos.top +
			(Nodes[target].Pos.bottom -
			 Nodes[target].Pos.top) /
			2;

		    MoveToEx(hDC, sx, sy, NULL);
		    LineTo(hDC, tx, ty);
		}
	    }
	}
    }
}

static Void local drawNode(HDC hDC, Node n)
{
    /* frame */
    Rectangle(hDC, Nodes[n].Pos.left - 4, Nodes[n].Pos.top - 2,
	  Nodes[n].Pos.right + 4, Nodes[n].Pos.bottom + 2);

    /* frame shadow */
    MoveToEx(hDC, Nodes[n].Pos.right + 4, Nodes[n].Pos.top, NULL);
    LineTo(hDC, Nodes[n].Pos.right + 4, Nodes[n].Pos.bottom + 2);
    LineTo(hDC, Nodes[n].Pos.left - 2, Nodes[n].Pos.bottom + 2);

    /* class text */
    fprintf(stdstr, "%s\n", textToStr(cclass(Nodes[n].Class).text));
    TextOut(hDC, Nodes[n].Pos.left, Nodes[n].Pos.top,
	(LPSTR) stdstrbuff, (INT) strlen(stdstrbuff));
}

typedef struct {
    HCURSOR hMoveClassCursor;
    HCURSOR hNormalCursor;
    Node SelectedClass;
    BOOL Moved;
    INT ClassesTopX, ClassesTopY;
    INT XOffset, YOffset;
    INT RealWidth, RealHeight;	/* size of window      */
    INT width, height;	/* size of total graph */
} ClassBrowserState;

static ClassBrowserState cBrowse;	/* state of browser */

static VOID local setClassBrowserSize()
{
    Node i;
    INT width = 0;
    INT height = 0;

    for (i = 0; i < LastNode; i++) {
	width = max(width, Nodes[i].Pos.right);
	height = max(height, Nodes[i].Pos.bottom);
    }
    cBrowse.width = width + 2 * GetSystemMetrics(SM_CXFRAME);
    cBrowse.height = height + 2 * GetSystemMetrics(SM_CYFRAME)
	+ GetSystemMetrics(SM_CYCAPTION);
    SetWindowPos(hWndClasses,
	     NULL,
	     0,
	     0,
	     cBrowse.width + GetSystemMetrics(SM_CXVSCROLL) + 10,
	     cBrowse.height + GetSystemMetrics(SM_CYHSCROLL) + 10,
	     SWP_NOMOVE | SWP_NOACTIVATE);
}

static Void local doGetMinMaxInfo_Classes(MINMAXINFO FAR * lpmmi)
{
    lpmmi->ptMinTrackSize.x = 50;
    lpmmi->ptMinTrackSize.y = 50;

    lpmmi->ptMaxTrackSize.x = MAX_WIDTH;
    lpmmi->ptMaxTrackSize.y = MAX_HEIGHT;

    lpmmi->ptMaxSize.x = MAX_WIDTH;
    lpmmi->ptMaxSize.y = MAX_HEIGHT;
}

static Void local doMouseMove_Classes(HWND hWnd, INT x, INT y)
{
    if (cBrowse.SelectedClass < 0) {
	SetCursor(cBrowse.hNormalCursor);
    } else {
	Node n = cBrowse.SelectedClass;
	INT dx, dy;
	RECT ClearRect;

	SetCursor(cBrowse.hMoveClassCursor);

	/* Don't allow move it out of window */
	x = max(5, min(cBrowse.RealWidth - 10, x));
	y = max(5, min(cBrowse.RealHeight - 10, y));

	dx = x - Nodes[n].Pos.left;
	dy = y - Nodes[n].Pos.top;

	ClearRect.left = Nodes[n].Pos.left - 5;
	ClearRect.right = Nodes[n].Pos.right + 5;
	ClearRect.top = Nodes[n].Pos.top - 3;
	ClearRect.bottom = Nodes[n].Pos.bottom + 3;

	InvalidateRect(hWnd, &ClearRect, FALSE);	/* erase old class */

	Nodes[n].Pos.left += dx;
	Nodes[n].Pos.top += dy;
	Nodes[n].Pos.right += dx;
	Nodes[n].Pos.bottom += dy;

	ClearRect.left = Nodes[n].Pos.left - 5;
	ClearRect.right = Nodes[n].Pos.right + 5;
	ClearRect.top = Nodes[n].Pos.top - 3;
	ClearRect.bottom = Nodes[n].Pos.bottom + 3;

	InvalidateRect(hWnd, &ClearRect, TRUE);	/* draw new class */

	SendMessage(hWnd, WM_PAINT, 0, 0L);
    }
}

#define clamp(_min,_max,x) max(_min,min(_max,x))

static Void local setOffset_Classes(HWND hWnd, INT x, INT y)
{
    Node n;
    INT dx, dy;

    x = clamp(-cBrowse.width, 0, x);
    y = clamp(-cBrowse.height, 0, y);
    dx = x - cBrowse.XOffset;
    dy = y - cBrowse.YOffset;

    for (n = 0; n < LastNode; n++) {
	Nodes[n].Pos.left += dx;
	Nodes[n].Pos.right += dx;
	Nodes[n].Pos.top += dy;
	Nodes[n].Pos.bottom += dy;
    }

    cBrowse.XOffset = x;
    cBrowse.YOffset = y;
    SetScrollPos(hWnd, SB_HORZ, -x, TRUE);
    SetScrollPos(hWnd, SB_VERT, -y, TRUE);

    ScrollWindow(hWnd, dx, dy, NULL, NULL);
    InvalidateRect(hWnd, NULL, TRUE);
    UpdateWindow(hWnd);
}

#undef clamp

static Void local lButtonDown_Classes(HWND hWnd, INT x, INT y)
{
    /* Select a class to drag it */
    Node n;

    for (n = 0; n < LastNode; n++) {
	if (Nodes[n].Pos.left - 4 < x && Nodes[n].Pos.right + 4 > x
	    && Nodes[n].Pos.top - 2 < y
	    && Nodes[n].Pos.bottom + 2 > y) {

	    SetCursor(cBrowse.hMoveClassCursor);
	    cBrowse.SelectedClass = n;

	    InvalidateRect(hWnd, NULL, TRUE);
	    SetCapture(hWnd);
	    return;
	}
    }
}

static Void local lButtonUp_Classes(HWND hWnd, INT x, INT y)
{
    if (cBrowse.SelectedClass >= 0) {
	Node n = cBrowse.SelectedClass;
	INT width = Nodes[n].Pos.right - Nodes[n].Pos.left;
	INT height = Nodes[n].Pos.bottom - Nodes[n].Pos.top;

	ReleaseCapture();

	if (cBrowse.Moved) {
	    Nodes[n].Pos.left = x;
	    Nodes[n].Pos.top = y;
	    Nodes[n].Pos.right = Nodes[n].Pos.left + width;
	    Nodes[n].Pos.bottom = Nodes[n].Pos.top + height;
	}

	cBrowse.SelectedClass = -1;
	cBrowse.Moved = FALSE;
	SetCursor(cBrowse.hNormalCursor);

	InvalidateRect(hWnd, NULL, TRUE);
	SendMessage(hWnd, WM_PAINT, 0, 0L);

	setClassBrowserSize();
    }
}

static Void local doPaint_Classes(HWND hWnd)
{
    PAINTSTRUCT ps;
    HDC hDC;
    HFONT hSaveFont;
    COLORREF SaveColor;
    Node i;

    hDC = BeginPaint(hWnd, &ps);

    /* Get font */
    hSaveFont = SelectObject(hDC, DefaultFont());
    SaveColor = SetTextColor(hDC, RGB(0, 0, 190));	/* Blue Color for text  */

    if (cBrowse.SelectedClass < 0) {	/* not dragging a class */
	drawClassRelations(hDC);
    }
    for (i = 0; i < LastNode; i++) {
	drawNode(hDC, i);
    }

    SetTextColor(hDC, SaveColor);	/* Restore color        */
    /* Restore font         */
    SelectObject(hDC, hSaveFont);

    EndPaint(hWnd, &ps);
}

static VOID local doCreate_Classes(HWND hWnd)
{
    PAINTSTRUCT ps;
    HDC hDC;
    HFONT hSaveFont;
    INT numClasses = classMax() - CLASSMIN;	/* total number of classes */

    cBrowse.hNormalCursor = LoadCursor(NULL, IDC_ARROW);
    cBrowse.hMoveClassCursor =
	LoadCursor(hThisInstance, "MOVECLASSCURSOR");
    cBrowse.SelectedClass = -1;
    cBrowse.Moved = FALSE;
    cBrowse.ClassesTopX = 10;
    cBrowse.ClassesTopY = 10;
    cBrowse.XOffset = 0;
    cBrowse.YOffset = 0;

    if (!allocNodes(numClasses)) {
	MessageBox(hWnd, "Out of memory: create nodes list", NULL,
	       MB_ICONEXCLAMATION | MB_OK);
	return;
    }

    hDC = BeginPaint(hWnd, &ps);

    hSaveFont = SelectObject(hDC, DefaultFont());

    buildClassGraph(hDC);

    /* Restore font */
    SelectObject(hDC, hSaveFont);

    EndPaint(hWnd, &ps);

    /* Show upper-left part of window */
    SetScrollPos(hWnd, SB_HORZ, 0, TRUE);
    SetScrollPos(hWnd, SB_VERT, 0, TRUE);

    setClassBrowserSize();
}

static Void local doDestroy_Classes()
{
    if (Nodes)
	free(Nodes);
    Nodes = NULL;
    LastNode = 0;
    DestroyCursor(cBrowse.hMoveClassCursor);
    hWndClasses = NULL;
}

static Void local doMove_Classes(HWND hWnd, INT x, INT y)
{
    /* WM_MOVE's coords are for the upper-left of the client area;
       we want the window's upper-left screen coords, so
       just use GetWindowRect() to get at this. */
    RECT r;

    GetWindowRect(hWndClasses, &r);
    cBrowse.ClassesTopX = r.left;
    cBrowse.ClassesTopY = r.top;
}

static Void local doSize_Classes(HWND hWnd, INT width, INT height)
{
    static BOOL RecursiveCall = FALSE;

    if (!RecursiveCall) {
	RecursiveCall = TRUE;

	cBrowse.RealWidth = width;
	cBrowse.RealHeight = height;

	if (cBrowse.RealWidth < cBrowse.width || cBrowse.XOffset) {
	    SetScrollRange(hWnd, SB_HORZ, 0, cBrowse.width,
		       TRUE);
	} else {	/* Hide scroll bar */
	    SetScrollRange(hWnd, SB_HORZ, 0, 0, TRUE);
	}

	if (cBrowse.RealHeight < cBrowse.height || cBrowse.YOffset) {
	    SetScrollRange(hWnd, SB_VERT, 0, cBrowse.height,
		       TRUE);
	} else {	/* Hide scroll bar */
	    SetScrollRange(hWnd, SB_VERT, 0, 0, TRUE);
	}

	RecursiveCall = FALSE;
    }
}

/* Hierarchy class window proc */

LRESULT CALLBACK ClassesWndProc(HWND hWnd, UINT msg, WPARAM wParam,
		LPARAM lParam)
{
    switch (msg) {
	case WM_CREATE:
	    doCreate_Classes(hWnd);
	    break;
	case WM_DESTROY:
	    doDestroy_Classes();
	    return (LRESULT) FALSE;
	case WM_GETMINMAXINFO:
	    doGetMinMaxInfo_Classes((MINMAXINFO FAR *) lParam);
	    break;
	case WM_SIZE:
	    doSize_Classes(hWnd, (INT) LOWORD(lParam),
		       (INT) HIWORD(lParam));
	    break;
	case WM_MOVE:
	    doMove_Classes(hWnd, (INT) LOWORD(lParam),
		       (INT) HIWORD(lParam));
	    break;
	case WM_HSCROLL:
	    switch (LOWORD(wParam)) {
		case SB_PAGEUP:
		case SB_LINEUP:
		    setOffset_Classes(hWnd, cBrowse.XOffset + 5,
			      cBrowse.YOffset);
		    break;
		case SB_PAGEDOWN:
		case SB_LINEDOWN:
		    setOffset_Classes(hWnd, cBrowse.XOffset + 5,
			      cBrowse.YOffset);
		    break;
		case SB_THUMBPOSITION:
		    setOffset_Classes(hWnd, -HIWORD(wParam),
			      cBrowse.YOffset);
		    break;
	    }
	    break;
	case WM_VSCROLL:
	    switch (LOWORD(wParam)) {
		case SB_PAGEUP:
		case SB_LINEUP:
		    setOffset_Classes(hWnd, cBrowse.XOffset,
			      cBrowse.YOffset + 5);
		    break;
		case SB_PAGEDOWN:
		case SB_LINEDOWN:
		    setOffset_Classes(hWnd, cBrowse.XOffset,
			      cBrowse.YOffset - 5);
		    break;
		case SB_THUMBPOSITION:
		    setOffset_Classes(hWnd, cBrowse.XOffset,
			      -HIWORD(wParam));
		    break;
	    }
	    break;
	case WM_PAINT:
	    doPaint_Classes(hWnd);
	    break;
	case WM_LBUTTONDOWN:
	    lButtonDown_Classes(hWnd, LOWORD(lParam), HIWORD(lParam));
	    break;
	case WM_LBUTTONUP:
	    lButtonUp_Classes(hWnd, LOWORD(lParam), HIWORD(lParam));
	    break;
	case WM_MOUSEMOVE:
	    doMouseMove_Classes(hWnd, LOWORD(lParam), HIWORD(lParam));
	    break;
	default:
	    return DefWindowProc(hWnd, msg, wParam, lParam);
    }
    return (LRESULT)TRUE;
}

/* Create class hierarchy and show it on a window */
void DrawClassesHierarchy()
{
    HWND hActiveWindow;
    RECT rActive, rWindow;
    WNDCLASS wc;

    hActiveWindow = GetActiveWindow();

    if (hWndClasses) {	/* If window exists keep its position */
	GetWindowRect(hWndClasses, &rWindow);
	DestroyWindow(hWndClasses);

    } else {
	GetWindowRect(hActiveWindow, &rActive);
	rWindow.top = rActive.top + 50;
	rWindow.left = rActive.left + 50;
    }

    wc.style = CS_VREDRAW | CS_HREDRAW;
    wc.lpfnWndProc = ClassesWndProc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance = hThisInstance;
    wc.hIcon = NULL;
    wc.hCursor = NULL;
    wc.hbrBackground = GetStockObject(LTGRAY_BRUSH);
    wc.lpszMenuName = NULL;
    wc.lpszClassName = "HugsClassesWindow";

    RegisterClass(&wc);

    hWndClasses =
	CreateWindow("HugsClassesWindow",
		 "Class Hierarchy",
		 WS_CAPTION | WS_BORDER | WS_SYSMENU |
		 WS_THICKFRAME | WS_VSCROLL | WS_HSCROLL,
		 rWindow.left, rWindow.top, 0, 0,
		 (HWND) hActiveWindow, (HMENU) NULL,
		 hThisInstance, (LPSTR) NULL);

    if (!hWndClasses) {
	ErrorBox("Error creating window");
	return;
    }

    setClassBrowserSize();
    ShowWindow(hWndClasses, SW_SHOWNORMAL);
    UpdateWindow(hWndClasses);

    SetFocus(hWndClasses);

    return;
}

// FROM WinHugs.c
void DoBrowseClasses()
{
    ExecDialog(hThisInstance, BROWSECLASSESDLGBOX,
	   BrowseClassesDlgProc);
}

/* Browse Type Constructors ... */
void DoBrowseTycons()
{
    ExecDialog(hThisInstance, BROWSETYCONSDLGBOX, BrowseTyconsDlgProc);
}

/* Browse Names ... */
void DoBrowseNames()
{
    ExecDialog(hThisInstance, BROWSENAMESDLGBOX, BrowseNamesDlgProc);
}

/*********************************************************
** SCRIPT MAN
*********************************************************/

INT_PTR CALLBACK ScriptManDlgProc(HWND hDlg, UINT msg, WPARAM wParam,
		  LPARAM lParam);

void ShowScriptMan()
{
    DialogBox(hThisInstance, MAKEINTRESOURCE(SCRIPTMANDLGBOX),
	  hThisWindow, ScriptManDlgProc);
}

/* --------------------------------------------------------------------------
 * Script Manager:
 * ------------------------------------------------------------------------*/

static INT smLoaded, smUpto;
static String smFile[NUM_SCRIPTS];
static INT selScr;

static Void local SmSelScr(HWND hDlg, Int i)
{
    selScr = i;
    SendDlgItemMessage(hDlg, LB_SCRIPTS, LB_SETCURSEL, i, 0L);
}

static Void local SmAddScr(HWND hDlg, CHAR * s)
{
    smFile[smUpto] = strCopy(s);
    fprintf(stdstr, "%s\n", smFile[smUpto]);
    SendDlgItemMessage(hDlg, LB_SCRIPTS, LB_ADDSTRING, 0,
	       (LONG) (LPSTR) stdstrbuff);
    SmSelScr(hDlg, smUpto);
    smUpto++;
}

INT_PTR CALLBACK ScriptManDlgProc(HWND hDlg, UINT msg,
		  WPARAM wParam, LPARAM lParam)
{
    switch (msg) {
	case WM_INITDIALOG:
	    {
		Int i;

		smLoaded = numLoadedScripts();
		smUpto = 0;

		CenterDialogInParent(hDlg);
		//SetDialogFont (hDlg, hDialogFont);

		SendDlgItemMessage(hDlg, LB_SCRIPTS,
			   LB_SETHORIZONTALEXTENT,
			   (WPARAM) 1000, 0L);

		SendDlgItemMessage(hDlg, LB_SCRIPTS, WM_SETREDRAW,
			   FALSE, 0L);

		for (i = 0; i < getScriptHwMark(); i++)
		    SmAddScr(hDlg, getScriptRealName(i));
		SmSelScr(hDlg, 0);
		SendDlgItemMessage(hDlg, LB_SCRIPTS, LB_SETCURSEL,
			   0, 0L);
		SendDlgItemMessage(hDlg, LB_SCRIPTS, WM_SETREDRAW,
			   TRUE, 0L);
		return TRUE;
	    }
	case WM_PAINT: {
		HDC hDC;
		PAINTSTRUCT Ps;
		HBITMAP hBitmap;
		RECT aRect, DlgRect;

		BeginPaint(hDlg, &Ps);
		hDC = Ps.hdc;

		/* Paint classes Bitmap */
		GetWindowRect(hDlg, &DlgRect);
		GetWindowRect(GetDlgItem(hDlg, ID_PLACEBITMAP),
			  &aRect);

		hBitmap =
		    LoadMappedBitmap(hThisInstance,
			     "SCRIPTMANDLGBMP");
		DrawBitmap(hDC, hBitmap,
		       aRect.left - DlgRect.left -
		       GetSystemMetrics(SM_CXDLGFRAME),
		       aRect.top - DlgRect.top -
		       GetSystemMetrics(SM_CYDLGFRAME) -
		       GetSystemMetrics(SM_CYCAPTION));
		DeleteObject(hBitmap);
		EndPaint(hDlg, &Ps);
	    }
	    break;

	case WM_COMMAND:
	    switch (CMDitem(wParam, lParam)) {
		case ID_ADDSCRIPT:
		    if (smUpto >= NUM_SCRIPTS)
			MessageBox(hDlg, "Too many script files",
			       "Add script",
			       MB_ICONEXCLAMATION | MB_OK);
		    else {
			CHAR Buffer[MAX_PATH];
			if (ShowOpenFileDialog(hDlg, Buffer))
			    SmAddScr(hDlg, Buffer);
		    }
		    return TRUE;

		case ID_DELSCRIPT:
		    if (selScr < 0)
			MessageBox(hDlg, "No script file selected",
			       "Remove script",
			       MB_ICONEXCLAMATION | MB_OK);
		    else if (selScr == 0)
			MessageBox(hDlg,
			       "Cannot remove prelude file",
			       "Remove script",
			       MB_ICONEXCLAMATION | MB_OK);
		    else {
			Int i;

			SendDlgItemMessage(hDlg, LB_SCRIPTS,
				   LB_DELETESTRING, selScr,
				   0L);
			if (selScr < smLoaded)
			    smLoaded = selScr;
			if (smFile[selScr]) {
			    free(smFile[selScr]);
			    smFile[selScr] = 0;
			}
			for (i = selScr + 1; i < smUpto; ++i)
			    smFile[i - 1] = smFile[i];
			smUpto--;
			SmSelScr(hDlg, -1);
		    }
		    return TRUE;

		case ID_EDITSCRIPT:
		    if (selScr >= 0)
			DlgSendMessage(hDlg, WM_COMMAND,
				   LB_SCRIPTS, MAKELONG(0,
					    LBN_DBLCLK));
		    else
			MessageBox(hDlg, "No file selected",
			       "Edit",
			       MB_ICONEXCLAMATION | MB_OK);
		    return TRUE;

		case ID_CLEARSCRIPTS: {
			Int i;

			for (i = 1; i < smUpto; ++i)
			    if (smFile[i])
				free(smFile[i]);
			smUpto = smLoaded = 1;
			SendDlgItemMessage(hDlg, LB_SCRIPTS,
				   LB_RESETCONTENT, 0, 0L);
			fprintf(stdstr, "%s\n", smFile[0]);
			SendDlgItemMessage(hDlg, LB_SCRIPTS,
				   LB_ADDSTRING, 0,
				   (LONG) (LPSTR)
				   stdstrbuff);
			SmSelScr(hDlg, -1);
			return TRUE;
		    }

		case LB_SCRIPTS:
		    switch (CMDdata(wParam, lParam)) {
		    case LBN_SELCHANGE:
			SmSelScr(hDlg,
			     (Int) SendDlgItemMessage(hDlg,
					  LB_SCRIPTS,
					  LB_GETCURSEL,
					  0, 0L));
			return TRUE;

		    case LBN_DBLCLK: {
			    char buffer[_MAX_PATH];

			    SendDlgItemMessage(hDlg,
				       LB_SCRIPTS,
				       LB_GETTEXT,
				       selScr,
				       (LPARAM) (LPSTR)
				       buffer);
			    setLastEdit((String) buffer, 0);
			    runEditor();
			    return TRUE;
			}
		    }
		    break;

		case IDOK: {
			Int i;

			/* Sigh, script stack hackery. */
			for (i = 0; i < getScriptHwMark(); i++)
			    if (getScriptName(i)) {
				free(getScriptName(i));
				free(getScriptRealName(i));
			    }
			for (i = 0; i < smUpto; i++) {
			    setScriptName(i, smFile[i]);
			    setScriptRealName(i,
				      strCopy(RealPath
					  (smFile
					   [i])));
			    smFile[i] = 0;
			}
			setScriptHwMark(smUpto);
			setNumLoadedScripts(smLoaded);
			dropScriptsFrom(smLoaded - 1);
			PostMessage(hThisWindow, WM_COMMAND,
				ID_COMPILE, 0L);
			EndDialog(hDlg, TRUE);
			return TRUE;
		    }

		case IDCANCEL:
		    {
			Int i;

			for (i = 0; i < smUpto; i++)
			    if (smFile[i])
				free(smFile[i]);
			EndDialog(hDlg, FALSE);
			return TRUE;
		    }
	    }
	    break;
    }
    return FALSE;
}
