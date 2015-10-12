#!/bin/sh

echo "\
Unable to generate a working configure script for djgpp

Running ./config.bat will copy hand-written versions of
  ./Makefile
  ./options.h
  ./config.h
into ..
"

# Actually, we can run the configure script - but on my machine it
# crashes while generating the output files.  The answers printed 
# before it crashed were used to hand-generate the output files.
#
# sed ../unix/configure >./config.fix            \
#   -e "s#/dev/null#conf_devnull#"
# 
# # Now we override the default values of some environment variables.
# 
# set -a # All modified env vars are to be exported!
# 
# CC=${CC="gcc"}
# 
# # Run the script
# 
# ./config.fix $*

# End




