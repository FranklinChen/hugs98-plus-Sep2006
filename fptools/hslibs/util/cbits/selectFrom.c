/*
 * (c) sof, 1999
 *
 * Stubs to help implement Select module.
 */

/* we're outside the realms of POSIX here... */
/* #include "PosixSource.h" */

#include "Rts.h"
#include "HsUtil.h"
#include <stdlib.h>
#include <string.h>

# if defined(HAVE_SYS_TYPES_H)
#  include <sys/types.h>
# endif

# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif

# ifdef HAVE_UNISTD_H
#  include <unistd.h>
# endif

/* Helpers for the Haskell-side unmarshalling */

StgInt
sizeof_fd_set__()
{
 return (sizeof(fd_set));
}

void
fd_zero__(StgByteArray a)
{
  FD_ZERO((fd_set*)a);
}

void
fd_set__(StgByteArray a, StgInt fd)
{
  FD_SET(fd,(fd_set*)a);
}

StgInt
is_fd_set__(StgByteArray a, StgInt fd)
{
  return FD_ISSET(fd,(fd_set*)a);
}

StgInt
selectFrom__( StgByteArray rfd
            , StgByteArray wfd
	    , StgByteArray efd
	    , StgInt mFd
	    , StgInt tout
	    )
{
 int rc;
 struct timeval tv;

 if (tout != (-1)) {
   tv.tv_sec = tout / 1000000;
   tv.tv_usec = tout % 1000000;
 }

 rc = select(mFd, (fd_set*)rfd, (fd_set*)wfd, (fd_set*)efd, 
	     (tout == -1 ? NULL : &tv));
 return rc;
}
