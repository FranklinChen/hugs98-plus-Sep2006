/*
 * Definitions for Epoc specific support functions
 * kept here to try to minimise impact on main source
 * files (hah!)
 *
 * Glenn Strong <Glenn.Strong@cs.tcd.ie>
*/
#include <signal.h>

/* signal() is declared in the Epoc libc, but apparently
 * not defined. Figure that one out if you can. */
extern _sig_func_ptr signal (int,_sig_func_ptr);
extern void strlwr(char *);

