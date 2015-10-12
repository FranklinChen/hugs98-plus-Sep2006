#!/bin/sh
# Configure script for Hugs (using Microsoft Visual C++)

# Before we can run the configure script, we have to patch some
# incompatabilities between Unix and Windows:
# 
# o Visual C++ can't handle the file descriptor that bash (from
#   cygwin beta release 16) passes to it when stdout is redirected
#   to /dev/null.
#
# o Visual C++ writes input filenames to stderr as it processes them.

sed ../unix/configure >./config.fix            \
  -e "s#/dev/null#conf_devnull#"               \
  -e "s/-v '\^ \*+'/-i \"error\\\|warning\"/g"

# Now we override the default values of some environment variables.

set -a # All modified env vars are to be exported!

CC=${CC="cl /nologo"}        
DEBUGFLAGS=${DEBUGFLAGS="-Zi"}
LDDEBUGFLAGS=${LDDEBUGFLAGS="-Zi"}
OPTFLAGS=${OPTFLAGS="-O2"}
CFLAGS=${CFLAGS="-ML"}   
LDFLAGS=$LD
DLL_FLAGS="/LD"
CPP=${CPP="cl /nologo /E"}   
LIBS=${LIBS="kernel32.lib advapi32.lib"}  
GUILIBS="kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib comctl32.lib winmm.lib advapi32.lib" 

# Run the script

./config.fix --target=windows $*

# Store the generated files for the benefit of those who can't
# run configure directly.

echo "Copying ../Makefile, ../config.h and ../options.h to ." 
cp ../Makefile ../config.h ../options.h . 

# End