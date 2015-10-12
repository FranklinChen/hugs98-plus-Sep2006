/* -----------------------------------------------------------------------------
 * $Id: PackedString.c,v 1.3 2002/12/11 16:39:00 simonmar Exp $
 *
 * PackedString C bits
 *
 * (c) The GHC Team 1998
 * -------------------------------------------------------------------------- */

#include "HsFFI.h"
#include "HsLang.h"

HsInt
byteArrayHasNUL__ (char *ba, HsInt len)
{
    HsInt i;

    for (i = 0; i < len; i++) {
	if (*(ba + i) == '\0') {
	    return(1); /* true */
	}
    }

    return(0); /* false */
}
