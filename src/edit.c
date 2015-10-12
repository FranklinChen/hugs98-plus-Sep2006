#include <math.h>
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "opts.h"
#include "strutil.h"
#include "machdep.h"

Bool startEdit(line,nm)    /* Start editor on file name at    */
Int    line;               /* given line.  Both name and line */
String nm; {               /* or just line may be zero        */
    String editorCmd;
    String fullNm;
    Bool   expandedName = FALSE;
    Bool   syncEdit     = TRUE;
    Bool   useShell     = FALSE;
    String he;
    String ec;
    unsigned int roomReqd = 0;
    unsigned int nmLen, lineLen, fullNmLen;
    
    /* First off, check whether we have actually got a plausible editor
     * available. On a Mac, files have creator information, telling which
     * program to launch to, so an editor named to the empty string ""
     * is often desirable.
     */
    if (!hugsEdit 
#if !(defined(macintosh))
        || *hugsEdit == '\0'
#endif
	             ) {
	ERRMSG(0) "Hugs is not configured to use an editor"
	EEND;
    } 
      
    /* More sanity checks */
    if (nm == NULL) {
      return FALSE;
    }
    
    fullNm = RealPath(nm);
    fullNmLen = strlen(fullNm);
    nmLen = strlen(nm);
    lineLen = 1 + (line == 0 ? 0 : (unsigned int)log10((double)line));
    
    he = hugsEdit;

    /* Compute the length of the expanded 'hugsEdit' string */
    while (*he) {
      if (*he++ == '%') {
	if (*he == 's') {
	  /* assume quotes are always put around the filename. */
	  roomReqd += nmLen + 2;
	  expandedName = TRUE;
	} else if (*he == 'f') {
	  /* assume quotes are always put around the filename. */
	  roomReqd += fullNmLen + 2;
	  expandedName = TRUE;
	} else if ( *he == 'd' ) {
	  roomReqd += lineLen;
	} else if ( *he == '%' ) {
	  /* %% is contracted to % in the expanded string */
	  roomReqd++;
	} else {
	  roomReqd += 2;
	}
	he++;
      } else {
	roomReqd++;
      }
    }
    
    if (!expandedName) {
      /* include room for quotes and an extra space */
      roomReqd += nmLen + 3;
    }
    
    editorCmd = (String)malloc(sizeof(char) * (roomReqd + 1));
    if (editorCmd == NULL) {
      Printf("Warning: Unable to start editor\n");
      return FALSE;
    }
    
    /* Given a properly sized output buffer, perform the expansion */
    expandedName = FALSE;
    ec = editorCmd;
    he = hugsEdit;
    
    /* If the editor command is prefixed with '&', the editor is
     * started up asynchronously (the default is for Hugs to block
     * and wait for the editor to exit).
     *
     * If the editor command is prefixed with '!', then the editor is
     * invoked by going via the shell.
     */
    while (1) {
      if (*he == '&') {
	syncEdit = FALSE;
	he++;
      } else if (*he == '!') {
	useShell = TRUE;
	he++;
      } else {
	break;
      }
    }
    
    while (*he) {
      if (*he=='%') {
	if (*++he=='d') {
	  sprintf(ec,"%d",(line ? line : 1));
	  ec += lineLen;
	  he++;
	} else if (*he == 's' || *he == 'f') {
	  /* Put quotes around it if the %s occurrence surrounded by wspace only. */
	  Bool useQuotes = isspace(he[-2]) && (he[1] == '\0' || isspace(he[1]));
	  if (useQuotes) *ec++='\"';
	  if (*he == 's') {
	    strcpy(ec,nm);
	    ec += nmLen;
	  } else {
	    strcpy(ec,fullNm);
	    ec += fullNmLen;
	  }
	  if (useQuotes) *ec++='\"';
	  *ec='\0';
	  expandedName = TRUE;
	  he++;
	} else if (*he == '%') { /* Unescape % */
	  *ec++ = '%';
	  he++;
	} else {
	  *ec++ = '%'; 
	  *ec++ = *he++;
	}
      } else {
	*ec++ = *he++;
      }
    }

    if (!expandedName) {  /* If file name was not included, add it. */
      *ec++=' ';
      /* Protect the filename by putting quotes around it */
      *ec++='\"'; strcpy(ec,nm); ec += nmLen; *ec++='\"';
    }
    
    /* Terminate the string and go! */
    *ec = '\0';  

    if (shellEsc(editorCmd,syncEdit/*sync*/, useShell/*sep console*/)) {
      Printf("Warning: Editor terminated abnormally\n");
      free(editorCmd);
      return FALSE;
    }
    free(editorCmd);
    return syncEdit;
}
