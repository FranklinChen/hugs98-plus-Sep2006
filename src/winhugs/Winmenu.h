/* --------------------------------------------------------------------------
 * WinMenu.h:	José Enrique Gallardo Ruiz, Feb 1999
 *
 * The Hugs 98 system is Copyright (c) José Enrique Gallardo, Mark P Jones,
 * Alastair Reid, the Yale Haskell Group, and the OGI School of
 * Science & Engineering at OHSU, 1994-2003, All rights reserved.  It is
 * distributed as free software under the license in the file "License",
 * which is included in the distribution.
 *
 * Defines for menus and dialog boxes
 * ------------------------------------------------------------------------*/
#ifndef __WINMENU_H__
#define __WINMENU_H__

#define ID_FILESMENU	  	  1
#define ID_EDITMENU	  	  2
#define ID_INTERPRETERMENU	  3
#define ID_OPTIONSMENU    	  10
#define ID_BROWSEMENU     	  4
#define ID_HELPMENU  	  	  5


#define ID_NEW  		101
#define ID_OPEN 		102
#define ID_SCRIPTMAN            103
#define ID_SAVE	        	104
#define ID_CLOSE		105
#define ID_PRINT		106
#define ID_OPENSELECTED		107
#define ID_EXIT	        	108

#define ID_COPY         	201
#define ID_PASTE        	202
#define ID_GOEDIT       	203
#define ID_CUT			204
#define ID_CLEAR		205
#define ID_FIND			206
#define ID_GOPREVIOUS           207
#define ID_GONEXT               208
#define ID_EDITSELECTED		209
#define ID_FONT       	        199

#define ID_RUN   		301
#define ID_STOP			302
#define ID_EVAL			303
#define ID_TYPE			304
#define ID_COMPILE		305
#define ID_MAKE			306
#define ID_CLEARALL		307
#define ID_INFO			308

#define ID_SETOPTIONS		401

#define ID_BROWSECLASSES	501
#define ID_BROWSENAMES		502
#define ID_BROWSETYCONS		503
#define ID_BROWSEHIERARCHY      504


#define ID_HELPINDEX		601
#define ID_HELPFIND		602
#define ID_HELPUSE		603
#define ID_ABOUT		604
#define ID_HELPCOMMANDS         605
#define ID_HELPREPORT		606
#define ID_HELPLIBS		607
#define ID_HELPGENTLE		608
#define ID_HELPDOCS		609
#define ID_HELPEXTS	        610
#define ID_HELPHASKELLORG       611


/* Dialog boxes */
#define ABOUTDLGBOX       	  1
#define OPTIONSDLGBOX     	  2
#define BROWSECLASSESDLGBOX	  3
#define BROWSENAMESDLGBOX	  4
#define BROWSETYCONSDLGBOX	  5
#define SCRIPTMANDLGBOX           6


#define IDS_FILTERPROJECT      2000
#define IDS_FILTERFILE         2001

/* Options Dialog */
#define ID_OP           	150
#define ID_PROMPT		122
#define ID_LASTEXPR		123
#define ID_EDITOR	        124
#define ID_PATH			125

#define ID_ROWS	                126
#define ID_COLS	                127
#define ID_HEAPSIZE	        128

#define ID_CUTOFF               129

/* Browse Classes dialog */
#define LB_CLASS	       1001
#define LB_INSTANCES	       1002
#define LB_MEMBERS	       1003
#define LB_CONTEXT	       1004
#define ID_HIERARCHY	       5001
#define ID_EDITCLASS	       5002
#define ID_EDITINSTANCE	       5003

/* Browse Names dialog */
#define LB_NAMES	       1001
#define LB_NAMESTYPE	       1002
#define LB_NAMESNOTES	       1003
#define IDC_SEARCHNAME	       1004
#define ID_EDITNAME            5001

/* Browse Tycons dialog */
#define LB_TYCONS	       1001
#define LB_CONS		       1002
#define LB_DEF		       1003
#define LB_TYCONSINST          1004
#define IDC_SEARCHTYCON	       1005
#define ID_EDITTYCON	       5001
#define ID_EDITTYCONSINST      5002

/* About Dialog */
#define ID_FREERESOURCES	1001
#define ID_TOTALMEMORY		1002


/* Script Manager dialog */
#define LB_SCRIPTS              1001
#define ID_ADDSCRIPT            5001
#define ID_DELSCRIPT            5002
#define ID_CLEARSCRIPTS         5003
#define ID_EDITSCRIPT           5004


#define ID_PLACEBITMAP		20000


#endif /* __WINMENU_H__ */
