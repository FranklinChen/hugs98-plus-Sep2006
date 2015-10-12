/*
 * Little helper to portably (i.e., across 'make's and command shells) print
 * out the current date, as needed by version.c
 *
 */
#include <stdio.h>
#include <time.h>

int
main()
{
    time_t t;
    struct tm *pt;
    char buf[256];
    
    if ( time(&t) == (-1) ) {
	fprintf(stderr, "Unable to get the current time.\n");
	fflush(stderr);
	return 1;
    }
    if ( (pt = localtime(&t)) == NULL ) {
	fprintf(stderr, "Unable to decode time value.\n");
	fflush(stderr);
	return 1;
    }
    if (strftime(buf, sizeof(buf), "%Y%m%d", pt) == 0) {
	fprintf(stderr, "Unable to format time string.\n");
	fflush(stderr);
	return 1;
    }
    printf("#define YYYYMMDD \"%s\"\n", buf);
    return 0;
}
