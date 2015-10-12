/*
 * Support functions for EPOC.
 * The EPOC libc is not complete, but that's OK. Here we plug the gaps.
 *
 * Glenn Strong <Glenn.Strong@cs.tcd.ie>
*/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
/*
 * Signals are not supported in EPOC, and probably never will be 
*/
_sig_func_ptr
signal(int x,_sig_func_ptr y){
        return 0;
};


/* 
 * convert string to lowercase.
 *
*/
void
strlwr(char *str){
	char *s;
	for(s=str;*s;s++)
		if(isupper(*s)) (*s)=(char)tolower(*s);
}


