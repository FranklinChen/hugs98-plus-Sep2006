#!/bin/sh
# Configure script for Hugs (using Borland bcc32)

# Before we can run the configure script, we have to patch some
# incompatabilities between Unix and Windows:
# 
# o Borland C sends error messages to stdout (instead of stderr).
#   Replacing uses of ">/dev/null" with ">conftest.out" (which is
#   where stderr is normally sent) hacks around this.
#
# o Borland C insists that "-o" options be of the form "-ofoo" 
#   - not -o foo.
#
# o Borland C writes input filenames to stderr as it processes them.
#
# o DOS truncates name to 8 characters which confuses conftestval 
#   with the program conftest

sed ../unix/configure >./config.fix            \
  -e "s#/dev/null#conftest.out#"    \
  -e "s/-o /-o/g"                   \
  -e "s/-v '\^ \*+'/-i error/g"     \
  -e "s/test -s conftest/test -s conftest.exe/g" \
  -e "s/conftestval/conftest.val/g"

# Now we override the default values of some environment variables.

set -a # All modified env vars are to be exported!

CC=${CC="bcc32"}              
DEBUGFLAGS=${DEBUGFLAGS="-v"}
LDDEBUGFLAGS=${LDDEBUGFLAGS="-v"}
OPTFLAGS=${OPTFLAGS="-d -y-"}
CFLAGS=${CFLAGS="-j5 -w-pia -w-aus -w-par -w-rvl*"} 
LDFLAGS=$LD
CPP=${CPP="cpp32"}            
DLL_FLAGS=${DLL_FLAGS="-WDE"} 

# Run the script

./config.fix --target=windows $*

# Now patch the Makefile - changing "-o foo" to "-ofoo".

sed -e "s/-o /-o/g" ../Makefile >../Makefile.patch 
mv -f ../Makefile.patch ../Makefile 

# End
