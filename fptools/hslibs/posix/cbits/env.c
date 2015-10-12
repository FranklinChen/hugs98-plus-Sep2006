/*
 * (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
 * 
 * \subsection[env.lc]{Environment Handling for LibPosix}
 * 
 * Many useful environment functions are not necessarily provided by libc.
 * To get around this problem, we introduce our own.  The first time that
 * you modify your environment, we copy the environment wholesale into
 * malloc'ed locations, so that subsequent modifications can do proper
 * memory management.  The $environ$ variable is updated with a pointer
 * to the current environment so that the normal $getenv$ and $exec*$ functions
 * should continue to work properly.
 */
#include "Rts.h"
#include "HsPosix.h"

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

/* Switch this on once we've moved the environment to the malloc arena */
int dirtyEnv = 0;

/* 
 * For some reason, OSF turns off the prototype for this if we're
 * _POSIX_SOURCE.  Seems to me that this ought to be an ANSI-ism
 * rather than a POSIX-ism, but no matter.  (JSM(?))
 */

char *
strDup(const char *src)
{
    int len = strlen(src) + 1;
    char *dst;

    if ((dst = malloc(len)) != NULL)
	memcpy(dst, src, len);
    return dst;
}

/* Replace the entire environment */
int
setenviron(char **envp)
{
    char **old = environ;
    int dirtyOld = dirtyEnv;
    int i;

    /* A quick hack to move the strings out of the heap */
    environ = envp;
    if (copyenv() != 0) {
	environ = old;
	errno = ENOMEM;
	return -1;
    }
    /* Release the old space if we allocated it ourselves earlier */
    if (dirtyOld) {
	for (i = 0; old[i] != NULL; i++)
	    free(old[i]);
	free(old);
    }
    return 0;
}

/* Copy initial environment into malloc arena */
int
copyenv(void)
{
    char **new;
    int i;

    for (i = 0; environ[i] != NULL; i++)
          ;

    if ((new = (char **) malloc((i + 1) * sizeof(char *))) == NULL) {
	errno = ENOMEM;
	return -1;
    }

    new[i] = NULL;

    while (--i >= 0) {
	if ((new[i] = strDup(environ[i])) == NULL) {
	    while (new[++i] != NULL)
		free(new[i]);
	    free(new);
	    errno = ENOMEM;
	    return -1;
	}
    }
    environ = new;
    dirtyEnv = 1;
    return 0;
}

/* Set or replace an environment variable 
 * simonm 14/2/96 - this is different to the standard C library 
 * implementation and the prototypes clash, so I'm calling it _setenv.
 */
int
_setenv(char *mapping)
{
    int i, keylen;
    char *p;
    char **new;

    /* We must have a non-empty key and an '=' */
    if (mapping[0] == '=' || (p = strchr(mapping, '=')) == NULL) {
	errno = EINVAL;
	return -1;
    }
    /* Include through the '=' for matching */
    keylen = p - mapping + 1;

    if (!dirtyEnv && copyenv() != 0)
	return -1;

    if ((p = strDup(mapping)) == NULL)
	return -1;

    /* Look for an existing key that matches */
    for (i = 0; environ[i] != NULL && strncmp(environ[i], p, keylen) != 0; i++);

    if (environ[i] != NULL) {
	free(environ[i]);
	environ[i] = p;
    } else {
	/* We want to grow the table by *two*, one for the new entry, one for the terminator */
	if ((new = (char **) realloc((void*)environ, (i + 2) * sizeof(char *))) == NULL) {
	    free(p);
	    return -1;
	}
	new[i] = p;
	new[i + 1] = NULL;
	environ = new;
    }
    return 0;
}

/* Delete a variable from the environment */
int
delenv(char *name)
{
    int i, keylen;

    if (strchr(name, '=') != NULL) {
	errno = EINVAL;
	return -1;
    }
    keylen = strlen(name);

    if (!dirtyEnv && copyenv() != 0)
	return -1;

    /* Look for a matching key */
    for (i = 0; environ[i] != NULL &&
      (strncmp(environ[i], name, keylen) != 0 || environ[i][keylen] != '='); i++);

    /* Don't complain if it wasn't there to begin with */
    if (environ[i] == NULL) {
	return 0;
    }
    free(environ[i]);

    do {
	environ[i] = environ[i + 1];
	i++;
    } while (environ[i] != NULL);

    return 0;
}
