/*
 * String utilities needed throughout the Hugs codebase.
 */ 
#ifndef __STRUTIL_H__
#define __STRUTIL_H__

/* string copy operator, allocates new via malloc() */
extern String strCopy Args((String));

/* substring copy operator, allocates new via malloc() */
extern String strnCopy Args((String, Int));

/* Given a string containing a possibly qualified name,
 * split it up into a module and a name portion.
 */
extern Void splitQualString Args((String, String*, String*));

/*
 * Auto-expandable string builder objects:
 */
typedef struct StringBuilder StringBuilder;

/*
 * Function: newStringBuilder(sz)
 *
 * Allocate a new StringBuilder object, giving its
 * initial buffer size 'sz'. If sz == 0, the initial size
 * is set to INITIAL_BUILDER_SIZE..
 *
 */
extern StringBuilder* newStringBuilder   Args((unsigned int sz));

/*
 * Function: freeStringBuilder(builder)
 *
 * Free up a StringBuilder object.
 */
extern void           freeStringBuilder  Args((StringBuilder* b));

/*
 * Function: prependString(builder, str)
 *
 * The preferred usage mode for StringBuilders is to
 * concatenate stuff at the back, but prependString()
 * handles addition at the other end.
 *
 * Returns TRUE if successful.
 */
extern Bool           prependString      Args((StringBuilder* b, char* str));

/*
 * Function: appendString(builder, str)
 *
 * Append 'str' to the back of 'builder', expanding its
 * buffer to accommodate the added string, if needs be.
 *
 * Returns TRUE if successful.
 */
extern Bool           appendString       Args((StringBuilder* b,  char* str));

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
extern Bool           appendStringFormat Args((StringBuilder* b, char* fmt, char* arg));

/*
 * Function: toString(builder)
 *
 * Returns a pointer to the builder's internal buffer, i.e.,
 * a copy is not made (=> the caller isn't passed the
 * responsibility of freeing the returned string.)
 *
 * 
 */
extern char*          toString           Args((StringBuilder* b));

#endif /* __STRUTIL_H__ */
