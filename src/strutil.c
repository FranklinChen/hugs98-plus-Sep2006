/*
 * String utilities needed throughout the Hugs codebase.
 */ 
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "strutil.h"
#include "char.h"

/* --------------------------------------------------------------------------
 * String manipulation routines:
 * ------------------------------------------------------------------------*/

String strCopy(s)         /* make malloced copy of a string   */
String s; {
    if (s) {
	char *t;
	if ((t=(char *)malloc(strlen(s)+1))==0) {
	    ERRMSG(0) "String storage space exhausted"
	    EEND;
	}
	strcpy(t, s);
	return t;
    }
    return NULL;
}

String strnCopy(s,n)      /* make malloced copy of a substring */
String s;
Int n; {
    if (s) {
	char *t;
	if ((Int)strlen(s) < n)
	    n = strlen(s);
	if ((t=(char *)malloc(n+1))==0) {
	    ERRMSG(0) "String storage space exhausted"
	    EEND;
	}
	strncpy(t, s, n);
	t[n] = '\0';
	return t;
    }
    return NULL;
}

/* Given a string containing a possibly qualified name,
 * split it up into a module and a name portion.
 */
Void splitQualString(nm, pMod, pName) 
String nm;
String* pMod;
String* pName; {
    String dot;

    /* Find the last occurrence of '.' preceded by an identifier */
    dot = nm + strlen(nm) - 1;
    while (dot != nm && !(*dot == '.' && isIn(dot[-1],IDAFTER)))
	dot--;

    if (dot == nm) {
	*pMod = NULL;
	*pName = nm;
    } else {
	/* The module portion consists of everything upto the last dot. */
	*pMod = strnCopy(nm, dot - nm);

	/* Everything after the last '.' is the name string */
	*pName = dot+1;
    }
}

/* Cheap&cheerful expandable strings / StringBuilders.
 *
 * Note: I'm willfully breaking the convention of using K&R style
 * function declarations here. Their time has come and gone.
 */
struct StringBuilder {
    unsigned int len;
    unsigned int size;
    char*        buf;
};

#define INITIAL_BUILDER_SIZE 200

static Bool expandStringBuilder Args((StringBuilder* b, unsigned int newSz));

/*
 * Function: newStringBuilder(sz)
 *
 * Allocate a new StringBuilder object, giving its
 * initial buffer size 'sz'. If sz == 0, the initial size
 * is set to INITIAL_BUILDER_SIZE..
 *
 */
StringBuilder*
newStringBuilder ( unsigned int sz )
{
    StringBuilder* b = (StringBuilder*)malloc(sizeof(StringBuilder));
    
    if (!b) return NULL;

    b->size = ( sz == 0 ? INITIAL_BUILDER_SIZE : sz);
    b->len  = 0;
    b->buf  = (char*)malloc(sizeof(char)*b->size);

    if (!b->buf) {
	free(b);
	return NULL;
    }
    *(b->buf) = '\0';
    return b;
}

/*
 * Function: expandStringBuilder(builder, newSz)
 *
 * Internal function for expanding a StringBuilder's internal
 * buffer to 'newSz'. Along with expansion the old contents of
 * the buffer is copied over. 
 * 
 * Returns TRUE if successful.
 */
static Bool
expandStringBuilder(StringBuilder* b, unsigned int newSz)
{
    char* buf;

    if (!b) return FALSE;
    if (b->size > newSz) return FALSE;

    buf = realloc(b->buf, newSz);
    if (!buf) return FALSE;
    b->size = newSz;
    b->buf  = buf;

   return TRUE;
}

/*
 * Function: prependString(builder, str)
 *
 * The preferred usage mode for StringBuilders is to
 * concatenate stuff at the back, but prependString()
 * handles addition at the other end.
 *
 * Returns TRUE if successful.
 */
Bool
prependString(StringBuilder* b, char* str)
{
    unsigned int len, newLen;
    
    if (!b) return FALSE;
    if (!str) return TRUE;

    len = strlen(str);
    newLen = b->len + len;
    if (len==0) return TRUE;
    
    if ( newLen >= b->size ) {
	unsigned int newSz = b->size*2;
	if ( newSz < newLen ) {
	    newSz = newLen;
	}
	if ( !expandStringBuilder(b, newSz) ) {
	    return FALSE;
	}
    }

    /* shift the buffer down to make room for 'str' */
    memcpy(b->buf+len, b->buf, b->len+1);
    memcpy(b->buf, str, len-1);
    b->len = newLen;
    
    return TRUE;
}

/*
 * Function: appendString(builder, str)
 *
 * Append 'str' to the back of 'builder', expanding its
 * buffer to accommodate the added string, if needs be.
 *
 * Returns TRUE if successful.
 */
Bool
appendString(StringBuilder* b, char* str)
{
    unsigned int len, newLen;
    
    if (!b)   return FALSE;
    if (!str) return TRUE;

    len = strlen(str);
    newLen = b->len + len;
    if (len==0) return TRUE;
    
    if ( newLen >= b->size ) {
	unsigned int newSz = b->size*2;
	if ( newSz < newLen ) {
	    newSz = newLen;
	}
	if ( !expandStringBuilder(b, newSz) ) {
	    return FALSE;
	}
    }

    memcpy(b->buf+b->len,str,len+1);
    b->len = newLen;
    
    return TRUE;
}

/*
 * Function: appendStringFormat(builder, fmt, arg)
 *
 * This function provides an extremely limited form of
 * sprintf()-style format strings, allowing the substitution
 * of 'arg' zero or more times via the format string 'fmt':
 *
 *   appendStringFormat(builder, "-P%s -Y%s", "foo");
 *
 * is equivalent to
 *
 *   appendString(builder, "-Pfoo -Yfoo");
 *
 * Returns TRUE if successful.
 */
Bool
appendStringFormat(StringBuilder* b, char* fmt, char* arg)
{
    char* prev = fmt;
    char* ptr = fmt;
    char tmp;
    
    if (!b) return FALSE;

    while ( (ptr = strchr(ptr, '%')) ) {
	/* Append verbatim text from the format string */
	tmp = *ptr;
	*ptr = '\0';
	appendString(b, prev);
	*ptr = tmp;

	if ( ptr[1] != '%' ) {
	    /* Not an escaped occurrence of '%' */
	    switch (ptr[1]) {
	    case 's': appendString(b, arg); ptr += 2; break;
	    default:
		fprintf(stderr,"Unsupported format specifier, %%%c; ignoring", ptr[1]);
		ptr += 2; break;
	    }
	} else {
	    if (ptr[1] != '\0') {
		ptr += 2;
	    }
	}
	prev = ptr;
    }
    appendString(b,prev);
    return TRUE;
}

/*
 * Function: toString(builder)
 *
 * Returns a pointer to the builder's internal buffer, i.e.,
 * a copy is not made (=> the caller isn't passed the
 * responsibility of freeing the returned string.)
 *
 * 
 */
char*
toString(StringBuilder* b)
{
    if (!b) return NULL;
    return (b->buf);
}

/*
 * Function: freeStringBuilder(builder)
 *
 * Free up a StringBuilder object.
 */
void
freeStringBuilder(StringBuilder* b)
{
    if (!b) return;
    if (b->buf) free(b->buf);
    b->buf = NULL;
    free(b);
}
