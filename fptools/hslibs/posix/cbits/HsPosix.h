/* -----------------------------------------------------------------------------
 * $Id: HsPosix.h,v 1.8 2005/03/08 04:45:38 wolfgang Exp $
 *
 * Definitions for package `posix' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef HSPOSIX_H
#define HSPOSIX_H

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif /* HAVE_SYS_WAIT_H */

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif /* HAVE_SIGNAL_H */

#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif /* HAVE_SYS_UTSNAME_H */

#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif /* HAVE_SYS_TIMES_H */

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif /* HAVE_DIRENT_H */

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#ifdef HAVE_UTIME_H
#include <utime.h>
#endif /* HAVE_UTIME_H */

#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif /* HAVE_TERMIOS_H */

#ifdef HAVE_GRP_H
#include <grp.h>
#endif /* HAVE_GRP_H */

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif /* HAVE_PWD_H */

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#include <dlfcn.h>

#ifndef _POSIX_VDISABLE
#define _POSIX_VDISABLE '\0'	/* Just a guess...but it works for Suns */
#endif

extern I_ nocldstop;

extern char *strDup(const char *);
extern char **environ;
extern int  setenviron(char **);
extern int  copyenv(void);
extern int  _setenv(char *);
extern int  delenv(char *);
extern void stg_sigaddset(StgByteArray newset, StgByteArray oldset, int signum);
extern void stg_sigdelset(StgByteArray newset, StgByteArray oldset, int signum);

#endif
